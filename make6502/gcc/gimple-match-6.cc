/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "gimple-match-auto.h"

bool
gimple_maybe_bit_not (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
      {
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { t };
	  {
	    res_ops[0] = captures[0];
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 8, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    default:;
    }
{
  tree t_pops[1];
  if (gimple_bit_not_with_nop (t, t_pops, valueize))
    {
      tree _p0 = t_pops[0];
      {
	tree captures[2] ATTRIBUTE_UNUSED = { t, _p0 };
	{
	  res_ops[0] = captures[0];
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 9, __FILE__, __LINE__, false);
	  return true;
	}
      }
    }
}
{
  tree t_pops[1];
  if (gimple_maybe_cmp (t, t_pops, valueize))
    {
      tree _p0 = t_pops[0];
      {
	tree captures[2] ATTRIBUTE_UNUSED = { t, _p0 };
	{
	  res_ops[0] = captures[0];
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 10, __FILE__, __LINE__, false);
	  return true;
	}
      }
    }
}
{
  tree t_pops[2];
  if (gimple_bit_xor_cst (t, t_pops, valueize))
    {
      tree _p0 = t_pops[0];
      tree _p1 = t_pops[1];
      {
	tree captures[3] ATTRIBUTE_UNUSED = { t, _p0, _p1 };
	{
	  res_ops[0] = captures[0];
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 11, __FILE__, __LINE__, false);
	  return true;
	}
      }
    }
}
  return false;
}

bool
gimple_with_certain_nonzero_bits2 (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
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
	      case BIT_IOR_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  switch (TREE_CODE (_p1))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
			  {
			    res_ops[0] = captures[0];
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 37, __FILE__, __LINE__, false);
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
    case INTEGER_CST:
      {
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { t };
	  {
	    res_ops[0] = captures[0];
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 38, __FILE__, __LINE__, false);
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
gimple_unsigned_integer_sat_add (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
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
	      case BIT_IOR_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree _p0_pops[2];
		    if (gimple_usadd_left_part_1 (_p0, _p0_pops, valueize))
		      {
			tree _q20 = _p0_pops[0];
			tree _q21 = _p0_pops[1];
			{
			  tree _p1_pops[2];
			  if (gimple_usadd_right_part_1 (_p1, _p1_pops, valueize))
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
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 47, __FILE__, __LINE__, false);
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
		    if (gimple_usadd_right_part_1 (_p0, _p0_pops, valueize))
		      {
			tree _q20 = _p0_pops[0];
			tree _q21 = _p0_pops[1];
			{
			  tree _p1_pops[2];
			  if (gimple_usadd_left_part_1 (_p1, _p1_pops, valueize))
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
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 47, __FILE__, __LINE__, false);
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
		    if (gimple_usadd_left_part_2 (_p0, _p0_pops, valueize))
		      {
			tree _q20 = _p0_pops[0];
			tree _q21 = _p0_pops[1];
			{
			  tree _p1_pops[2];
			  if (gimple_usadd_right_part_2 (_p1, _p1_pops, valueize))
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
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 48, __FILE__, __LINE__, false);
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
		    if (gimple_usadd_right_part_2 (_p0, _p0_pops, valueize))
		      {
			tree _q20 = _p0_pops[0];
			tree _q21 = _p0_pops[1];
			{
			  tree _p1_pops[2];
			  if (gimple_usadd_left_part_2 (_p1, _p1_pops, valueize))
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
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 48, __FILE__, __LINE__, false);
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
			      case GE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  {
				    tree _q20_pops[2];
				    if (gimple_usadd_left_part_1 (_q20, _q20_pops, valueize))
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
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 49, __FILE__, __LINE__, false);
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
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  {
				    tree _q20_pops[2];
				    if (gimple_usadd_left_part_1 (_q20, _q20_pops, valueize))
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
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 50, __FILE__, __LINE__, false);
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
								  case CFN_ADD_OVERFLOW:
								    if (gimple_call_num_args (_c4) == 2)
								      {
									tree _q40 = gimple_call_arg (_c4, 0);
									_q40 = do_valueize (valueize, _q40);
									tree _q41 = gimple_call_arg (_c4, 1);
									_q41 = do_valueize (valueize, _q41);
									if (integer_zerop (_q21))
									  {
									    {
									      tree _p1_pops[2];
									      if (gimple_usadd_left_part_2 (_p1, _p1_pops, valueize))
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
												  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
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
												  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
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
								  case CFN_ADD_OVERFLOW:
								    if (gimple_call_num_args (_c4) == 2)
								      {
									tree _q40 = gimple_call_arg (_c4, 0);
									_q40 = do_valueize (valueize, _q40);
									tree _q41 = gimple_call_arg (_c4, 1);
									_q41 = do_valueize (valueize, _q41);
									if (integer_zerop (_q21))
									  {
									    if (integer_minus_onep (_p1))
									      {
										{
										  tree _p2_pops[2];
										  if (gimple_usadd_left_part_2 (_p2, _p2_pops, valueize))
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
												  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 52, __FILE__, __LINE__, false);
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
												  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 52, __FILE__, __LINE__, false);
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
											case SSA_NAME:
											  if (gimple *_d5 = get_def (valueize, _p2))
											    {
											      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
											        switch (gimple_assign_rhs_code (_a5))
												  {
												  case REALPART_EXPR:
												    {
												      tree _q90 = TREE_OPERAND (gimple_assign_rhs1 (_a5), 0);
												      if ((TREE_CODE (_q90) == SSA_NAME
												           || is_gimple_min_invariant (_q90)))
												        {
													  _q90 = do_valueize (valueize, _q90);
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
														      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 53, __FILE__, __LINE__, false);
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
			      case LE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  {
				    tree _q21_pops[2];
				    if (gimple_usadd_left_part_1 (_q21, _q21_pops, valueize))
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
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 54, __FILE__, __LINE__, false);
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
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  {
				    tree _q21_pops[2];
				    if (gimple_usadd_left_part_1 (_q21, _q21_pops, valueize))
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
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 55, __FILE__, __LINE__, false);
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
				{
				  tree _q20_pops[2];
				  if (gimple_usadd_left_part_1 (_q20, _q20_pops, valueize))
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
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 49, __FILE__, __LINE__, false);
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
		      }
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
				{
				  tree _q20_pops[2];
				  if (gimple_usadd_left_part_1 (_q20, _q20_pops, valueize))
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
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 50, __FILE__, __LINE__, false);
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
		      }
		    case EQ_EXPR:
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
								case CFN_ADD_OVERFLOW:
								  if (gimple_call_num_args (_c3) == 2)
								    {
								      tree _q40 = gimple_call_arg (_c3, 0);
								      _q40 = do_valueize (valueize, _q40);
								      tree _q41 = gimple_call_arg (_c3, 1);
								      _q41 = do_valueize (valueize, _q41);
								      if (integer_zerop (_q21))
									{
									  {
									    tree _p1_pops[2];
									    if (gimple_usadd_left_part_2 (_p1, _p1_pops, valueize))
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
												if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
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
												if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
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
								case CFN_ADD_OVERFLOW:
								  if (gimple_call_num_args (_c3) == 2)
								    {
								      tree _q40 = gimple_call_arg (_c3, 0);
								      _q40 = do_valueize (valueize, _q40);
								      tree _q41 = gimple_call_arg (_c3, 1);
								      _q41 = do_valueize (valueize, _q41);
								      if (integer_zerop (_q21))
									{
									  if (integer_minus_onep (_p1))
									    {
									      {
										tree _p2_pops[2];
										if (gimple_usadd_left_part_2 (_p2, _p2_pops, valueize))
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
												if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 52, __FILE__, __LINE__, false);
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
												if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 52, __FILE__, __LINE__, false);
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
										      case SSA_NAME:
										        if (gimple *_d4 = get_def (valueize, _p2))
										          {
											    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
											      switch (gimple_assign_rhs_code (_a4))
												{
												case REALPART_EXPR:
												  {
												    tree _q90 = TREE_OPERAND (gimple_assign_rhs1 (_a4), 0);
												    if ((TREE_CODE (_q90) == SSA_NAME
												         || is_gimple_min_invariant (_q90)))
												      {
													_q90 = do_valueize (valueize, _q90);
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
														    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 53, __FILE__, __LINE__, false);
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
		    case LE_EXPR:
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
				{
				  tree _q21_pops[2];
				  if (gimple_usadd_left_part_1 (_q21, _q21_pops, valueize))
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
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 54, __FILE__, __LINE__, false);
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
		      }
		    case GT_EXPR:
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
				{
				  tree _q21_pops[2];
				  if (gimple_usadd_left_part_1 (_q21, _q21_pops, valueize))
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
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 55, __FILE__, __LINE__, false);
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
		      }
		    default:;
		    }
	          break;
	        }
	      case PLUS_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  switch (TREE_CODE (_p0))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p0))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case MIN_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
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
							      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
			      case GE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  {
				    tree _q20_pops[2];
				    if (gimple_usadd_left_part_1 (_q20, _q20_pops, valueize))
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
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 49, __FILE__, __LINE__, false);
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
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  {
				    tree _q20_pops[2];
				    if (gimple_usadd_left_part_1 (_q20, _q20_pops, valueize))
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
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 50, __FILE__, __LINE__, false);
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
								  case CFN_ADD_OVERFLOW:
								    if (gimple_call_num_args (_c4) == 2)
								      {
									tree _q40 = gimple_call_arg (_c4, 0);
									_q40 = do_valueize (valueize, _q40);
									tree _q41 = gimple_call_arg (_c4, 1);
									_q41 = do_valueize (valueize, _q41);
									if (integer_zerop (_q21))
									  {
									    {
									      tree _p1_pops[2];
									      if (gimple_usadd_left_part_2 (_p1, _p1_pops, valueize))
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
												  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
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
												  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
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
								  case CFN_ADD_OVERFLOW:
								    if (gimple_call_num_args (_c4) == 2)
								      {
									tree _q40 = gimple_call_arg (_c4, 0);
									_q40 = do_valueize (valueize, _q40);
									tree _q41 = gimple_call_arg (_c4, 1);
									_q41 = do_valueize (valueize, _q41);
									if (integer_zerop (_q21))
									  {
									    if (integer_minus_onep (_p1))
									      {
										{
										  tree _p2_pops[2];
										  if (gimple_usadd_left_part_2 (_p2, _p2_pops, valueize))
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
												  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 52, __FILE__, __LINE__, false);
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
												  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 52, __FILE__, __LINE__, false);
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
											case SSA_NAME:
											  if (gimple *_d5 = get_def (valueize, _p2))
											    {
											      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
											        switch (gimple_assign_rhs_code (_a5))
												  {
												  case REALPART_EXPR:
												    {
												      tree _q90 = TREE_OPERAND (gimple_assign_rhs1 (_a5), 0);
												      if ((TREE_CODE (_q90) == SSA_NAME
												           || is_gimple_min_invariant (_q90)))
												        {
													  _q90 = do_valueize (valueize, _q90);
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
														      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 53, __FILE__, __LINE__, false);
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
			      case LE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  {
				    tree _q21_pops[2];
				    if (gimple_usadd_left_part_1 (_q21, _q21_pops, valueize))
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
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 54, __FILE__, __LINE__, false);
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
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  {
				    tree _q21_pops[2];
				    if (gimple_usadd_left_part_1 (_q21, _q21_pops, valueize))
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
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 55, __FILE__, __LINE__, false);
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
				{
				  tree _q20_pops[2];
				  if (gimple_usadd_left_part_1 (_q20, _q20_pops, valueize))
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
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 49, __FILE__, __LINE__, false);
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
		      }
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
				{
				  tree _q20_pops[2];
				  if (gimple_usadd_left_part_1 (_q20, _q20_pops, valueize))
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
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 50, __FILE__, __LINE__, false);
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
		      }
		    case EQ_EXPR:
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
								case CFN_ADD_OVERFLOW:
								  if (gimple_call_num_args (_c3) == 2)
								    {
								      tree _q40 = gimple_call_arg (_c3, 0);
								      _q40 = do_valueize (valueize, _q40);
								      tree _q41 = gimple_call_arg (_c3, 1);
								      _q41 = do_valueize (valueize, _q41);
								      if (integer_zerop (_q21))
									{
									  {
									    tree _p1_pops[2];
									    if (gimple_usadd_left_part_2 (_p1, _p1_pops, valueize))
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
												if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
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
												if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
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
								case CFN_ADD_OVERFLOW:
								  if (gimple_call_num_args (_c3) == 2)
								    {
								      tree _q40 = gimple_call_arg (_c3, 0);
								      _q40 = do_valueize (valueize, _q40);
								      tree _q41 = gimple_call_arg (_c3, 1);
								      _q41 = do_valueize (valueize, _q41);
								      if (integer_zerop (_q21))
									{
									  if (integer_minus_onep (_p1))
									    {
									      {
										tree _p2_pops[2];
										if (gimple_usadd_left_part_2 (_p2, _p2_pops, valueize))
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
												if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 52, __FILE__, __LINE__, false);
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
												if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 52, __FILE__, __LINE__, false);
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
										      case SSA_NAME:
										        if (gimple *_d4 = get_def (valueize, _p2))
										          {
											    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
											      switch (gimple_assign_rhs_code (_a4))
												{
												case REALPART_EXPR:
												  {
												    tree _q90 = TREE_OPERAND (gimple_assign_rhs1 (_a4), 0);
												    if ((TREE_CODE (_q90) == SSA_NAME
												         || is_gimple_min_invariant (_q90)))
												      {
													_q90 = do_valueize (valueize, _q90);
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
														    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 53, __FILE__, __LINE__, false);
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
		    case LE_EXPR:
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
				{
				  tree _q21_pops[2];
				  if (gimple_usadd_left_part_1 (_q21, _q21_pops, valueize))
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
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 54, __FILE__, __LINE__, false);
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
		      }
		    case GT_EXPR:
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
				{
				  tree _q21_pops[2];
				  if (gimple_usadd_left_part_1 (_q21, _q21_pops, valueize))
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
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 55, __FILE__, __LINE__, false);
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
gimple_simplify_111 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail712;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 226, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail712:;
  return false;
}

bool
gimple_simplify_114 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[1])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail715;
  {
    res_op->set_op (BIT_IOR_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    {
      tree _o1[1], _r1;
      _o1[0] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail715;
      res_op->ops[1] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 228, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail715:;
  return false;
}

bool
gimple_simplify_119 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail720;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 232, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail720:;
  return false;
}

bool
gimple_simplify_124 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[1])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail725;
  {
    res_op->set_op (bitop, type, 2);
    res_op->ops[0] = captures[0];
    {
      tree _o1[1], _r1;
      _o1[0] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail725;
      res_op->ops[1] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 235, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail725:;
  return false;
}

bool
gimple_simplify_132 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail733;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 231, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail733:;
  return false;
}

bool
gimple_simplify_135 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail737;
	  {
	    res_op->set_op (op, type, 2);
	    res_op->ops[0] = captures[0];
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[6];
	      if (type != TREE_TYPE (_o1[0])
	          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
		{
		  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail737;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 242, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail737:;
	}
      else
	{
	  if (single_use (captures[0]) && single_use (captures[1])
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail738;
	      {
		res_op->set_op (op, type, 2);
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[3];
		  if (type != TREE_TYPE (_o1[0])
		      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail738;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] = captures[4];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 243, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail738:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_141 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail746;
  {
    res_op->set_op (bitop, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[2];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 247, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail746:;
  return false;
}

bool
gimple_simplify_146 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitop == BIT_AND_EXPR
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail751;
      {
	tree tem;
	tem =  build_zero_cst (type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 249, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail751:;
    }
  else
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail752;
      {
	tree tem;
	tem =  build_minus_one_cst (type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 250, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail752:;
    }
  return false;
}

bool
gimple_simplify_149 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && invert_tree_comparison (cmp, HONOR_NANS (captures[1])) == icmp
 && canonicalize_math_after_vectorization_p ()
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail755;
      {
	res_op->set_op (BIT_AND_EXPR, type, 2);
	{
	  tree _o1[3], _r1;
	  _o1[0] = captures[0];
	  _o1[1] = captures[3];
	  _o1[2] = captures[5];
	  gimple_match_op tem_op (res_op->cond.any_else (), COND_EXPR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail755;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] =  build_one_cst (type);
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 251, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail755:;
    }
  return false;
}

bool
gimple_simplify_151 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])
          || !single_use (captures[3])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail760;
  {
    res_op->set_op (BIT_XOR_EXPR, type, 2);
    {
      tree _o1[2], _r1;
      {
	tree _o2[2], _r2;
	_o2[0] = captures[1];
	_o2[1] = captures[4];
	gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	tem_op.resimplify (lseq, valueize);
	_r2 = maybe_push_res_to_seq (&tem_op, lseq);
	if (!_r2) goto next_after_fail760;
	_o1[0] = _r2;
      }
      _o1[1] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail760;
      res_op->ops[0] = _r1;
    }
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 256, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail760:;
  return false;
}

bool
gimple_simplify_157 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail778;
		  {
		    tree tem;
		    tem = captures[3];
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 267, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail778:;
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail779;
		  {
		    tree tem;
		    tem = captures[0];
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 268, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail779:;
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
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail780;
		      {
			tree tem;
			tem = captures[3];
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 269, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail780:;
		    }
		  else
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail781;
		      {
			tree tem;
			tem = captures[0];
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 270, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail781:;
		    }
		}
	      else
		{
		  if (cmp == 0
 && ((code1 == LT_EXPR && code2 == GT_EXPR)
 || (code1 == GT_EXPR && code2 == LT_EXPR))
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail782;
		      {
			res_op->set_op (NE_EXPR, type, 2);
			res_op->ops[0] = captures[1];
			res_op->ops[1] = captures[4];
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 271, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail782:;
		    }
		  else
		    {
		      if (cmp >= 0
 && (code1 == LT_EXPR || code1 == LE_EXPR)
 && (code2 == GT_EXPR || code2 == GE_EXPR)
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail783;
			  {
			    tree tem;
			    tem =  constant_boolean_node (true, type);
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 272, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail783:;
			}
		      else
			{
			  if (cmp <= 0
 && (code1 == GT_EXPR || code1 == GE_EXPR)
 && (code2 == LT_EXPR || code2 == LE_EXPR)
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail784;
			      {
				tree tem;
				tem =  constant_boolean_node (true, type);
				res_op->set_value (tem);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 273, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail784:;
			    }
			}
		    }
		}
	    }
      }
    }
  return false;
}

bool
gimple_simplify_165 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail799;
	      {
		tree tem;
		tem = captures[3];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 260, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail799:;
	    }
	  else
	    {
	      if (code1 == NE_EXPR && val && allbits
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail800;
		  {
		    tree tem;
		    tem =  constant_boolean_node (true, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 261, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail800:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail801;
		      {
			tree tem;
			tem = captures[0];
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 262, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail801:;
		    }
		  else
		    {
		      if (code1 == EQ_EXPR
 && code2 == GT_EXPR
 && cmp == 0
 && allbits
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail802;
			  {
			    res_op->set_op (GE_EXPR, type, 2);
			    res_op->ops[0] = captures[4];
			    res_op->ops[1] = captures[5];
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 263, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail802:;
			}
		      else
			{
			  if (code1 == EQ_EXPR
 && code2 == LT_EXPR
 && cmp == 0
 && allbits
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail803;
			      {
				res_op->set_op (LE_EXPR, type, 2);
				res_op->ops[0] = captures[4];
				res_op->ops[1] = captures[5];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 264, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail803:;
			    }
			  else
			    {
			      if (code1 == EQ_EXPR
 && code2 == GE_EXPR
 && one_before
 && allbits
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail804;
				  {
				    res_op->set_op (GE_EXPR, type, 2);
				    res_op->ops[0] = captures[4];
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[2];
				      if (TREE_TYPE (res_op->ops[0]) != TREE_TYPE (_o1[0])
				          && !useless_type_conversion_p (TREE_TYPE (res_op->ops[0]), TREE_TYPE (_o1[0])))
					{
					  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (res_op->ops[0]), _o1[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail804;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 265, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail804:;
				}
			      else
				{
				  if (code1 == EQ_EXPR
 && code2 == LE_EXPR
 && one_after
 && allbits
)
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail805;
				      {
					res_op->set_op (LE_EXPR, type, 2);
					res_op->ops[0] = captures[4];
					{
					  tree _o1[1], _r1;
					  _o1[0] = captures[2];
					  if (TREE_TYPE (res_op->ops[0]) != TREE_TYPE (_o1[0])
					      && !useless_type_conversion_p (TREE_TYPE (res_op->ops[0]), TREE_TYPE (_o1[0])))
					    {
					      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (res_op->ops[0]), _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail805;
					    }
					  else
					    _r1 = _o1[0];
					  res_op->ops[1] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 266, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail805:;
				    }
				}
			    }
			}
		    }
		}
	    }
      }
    }
  return false;
}

bool
gimple_simplify_184 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail825;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail825;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 283, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail825:;
    }
  return false;
}

bool
gimple_simplify_189 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail830;
      {
	res_op->set_op (ABS_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 284, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail830:;
    }
  return false;
}

bool
gimple_simplify_193 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail834;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 286, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail834:;
  return false;
}

bool
gimple_simplify_197 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail838;
  {
    tree tem;
    tem =  build_zero_cst (type);
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 289, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail838:;
  return false;
}

bool
gimple_simplify_202 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail845;
	  {
	    res_op->set_op (COND_EXPR, type, 3);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] = captures[3];
	    res_op->ops[2] =  build_zero_cst (type);
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 293, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail845:;
	}
    }
  return false;
}

bool
gimple_simplify_206 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SQRT_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      if (!tree_expr_maybe_signaling_nan_p (captures[1])
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail850;
	  {
	    tree tem;
	    tem = captures[1];
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 298, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail850:;
	}
    }
  return false;
}

bool
gimple_simplify_209 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POW))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[3])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail853;
      {
	res_op->set_op (POW, type, 2);
	res_op->ops[0] = captures[1];
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[2];
	  _o1[1] = captures[4];
	  gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail853;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 301, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail853:;
    }
  return false;
}

bool
gimple_simplify_213 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail857;
  {
    tree tem;
    tem =  constant_boolean_node (op == NE_EXPR ? true : false, type);
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 305, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail857:;
  return false;
}

bool
gimple_simplify_216 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0]))
 && (TREE_INT_CST_LOW (captures[1]) & 1) != 0
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail860;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 308, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail860:;
    }
  return false;
}

bool
gimple_simplify_218 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail865;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[2];
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[2]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 313, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail865:;
    }
  return false;
}

bool
gimple_simplify_222 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
 && tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[2]))
 && (CONSTANT_CLASS_P (captures[3]) || (single_use (captures[1]) && single_use (captures[0])))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail882;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[3];
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[3]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 329, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail882:;
    }
  return false;
}

bool
gimple_simplify_225 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[1]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail885;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[3];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 332, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail885:;
    }
  return false;
}

bool
gimple_simplify_228 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (neeq))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 poly_int64 diff; tree inner_type = TREE_TYPE (captures[1]);
      if (ptr_difference_const (captures[0], captures[2], &diff)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail888;
	  {
	    res_op->set_op (neeq, type, 2);
	    {
	      tree _o1[2], _r1;
	      _o1[0] =  build_int_cst_type (inner_type, diff);
	      _o1[1] = captures[1];
	      gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail888;
	      res_op->ops[0] = _r1;
	    }
	    res_op->ops[1] = captures[3];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 334, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail888:;
	}
  }
  return false;
}

bool
gimple_simplify_233 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (out))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail893;
      {
	res_op->set_op (out, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 339, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail893:;
    }
  return false;
}

bool
gimple_simplify_237 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail900;
	  {
	    tree tem;
	    tem =  constant_boolean_node (cmp == NE_EXPR, type);
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 346, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail900:;
	}
      else
	{
	  {
 wide_int c1 = wi::to_wide (captures[3]);
 wide_int c2 = wi::lrshift (wi::to_wide (captures[4]), c1);
 wide_int c3 = wi::lrshift (wi::to_wide (captures[5]), c1);
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])
	              || !single_use (captures[1])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail901;
	      {
		res_op->set_op (cmp, type, 2);
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] =  wide_int_to_tree (TREE_TYPE (captures[2]), c2);
		  gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail901;
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] =  wide_int_to_tree (TREE_TYPE (captures[2]), c3);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 347, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail901:;
	  }
	}
    }
  return false;
}

bool
gimple_simplify_242 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail913;
	  {
	    tree tem;
	    tem =  constant_boolean_node (cmp == NE_EXPR, type);
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 359, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail913:;
	}
      else
	{
	  if (!integer_zerop (captures[2])
 && wi::lshift (wi::to_wide (captures[0]), cand) == wi::to_wide (captures[2])
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail914;
	      {
		res_op->set_op (cmp, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] =  build_int_cst (TREE_TYPE (captures[1]), cand);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 360, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail914:;
	    }
	}
  }
  return false;
}

bool
gimple_simplify_247 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree ctype = TREE_TYPE (captures[0]);
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail919;
      {
	res_op->set_op (cmp, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  if (ctype != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (ctype, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, ctype, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail919;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[2];
	  if (ctype != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (ctype, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, ctype, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail919;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 365, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail919:;
  }
  return false;
}

bool
gimple_simplify_251 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (single_use (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail924;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[1];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 370, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail924:;
    }
  return false;
}

bool
gimple_simplify_255 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (! FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 || ! tree_expr_maybe_nan_p (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail932;
      {
	tree tem;
	tem =  constant_boolean_node (true, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 377, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail932:;
    }
  else
    {
      if (cmp != EQ_EXPR
 && (! FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 || ! flag_trapping_math)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail933;
	  {
	    res_op->set_op (EQ_EXPR, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] = captures[0];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 378, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail933:;
	}
    }
  return false;
}

bool
gimple_simplify_260 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (captures[1]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail938;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] =  build_real (TREE_TYPE (captures[1]), dconst0);
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 382, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail938:;
    }
  else
    {
      if (TREE_CODE (captures[0]) == REAL_CST
 && REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (captures[0]))
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail939;
	  {
	    res_op->set_op (cmp, type, 2);
	    res_op->ops[0] =  build_real (TREE_TYPE (captures[0]), dconst0);
	    res_op->ops[1] = captures[1];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 383, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail939:;
	}
      else
	{
	  if (REAL_VALUE_ISNAN (TREE_REAL_CST (captures[1]))
 && (cmp == EQ_EXPR || cmp == NE_EXPR || !flag_trapping_math)
 && !tree_expr_signaling_nan_p (captures[1])
 && !tree_expr_maybe_signaling_nan_p (captures[0])
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail940;
	      {
		tree tem;
		tem =  constant_boolean_node (cmp == NE_EXPR, type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 384, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail940:;
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail941;
		  {
		    tree tem;
		    tem =  constant_boolean_node (cmp == NE_EXPR, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 385, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail941:;
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
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail942;
			      {
				tree tem;
				tem =  constant_boolean_node (false, type);
				res_op->set_value (tem);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 386, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail942:;
			    }
			  else
			    {
			      if (code == LE_EXPR
)
				{
				  if (! HONOR_NANS (captures[0])
)
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail943;
				      {
					tree tem;
					tem =  constant_boolean_node (true, type);
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 387, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail943:;
				    }
				  else
				    {
				      if (!flag_trapping_math
)
					{
					  gimple_seq *lseq = seq;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail944;
					  {
					    res_op->set_op (EQ_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    res_op->ops[1] = captures[0];
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 388, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail944:;
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
					      gimple_seq *lseq = seq;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail945;
					      {
						res_op->set_op (LT_EXPR, type, 2);
						res_op->ops[0] = captures[0];
						res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), max);
						res_op->resimplify (lseq, valueize);
						if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 389, __FILE__, __LINE__, true);
						return true;
					      }
next_after_fail945:;
					    }
					  else
					    {
					      gimple_seq *lseq = seq;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail946;
					      {
						res_op->set_op (GT_EXPR, type, 2);
						res_op->ops[0] = captures[0];
						res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), max);
						res_op->resimplify (lseq, valueize);
						if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 390, __FILE__, __LINE__, true);
						return true;
					      }
next_after_fail946:;
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
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail947;
						  {
						    res_op->set_op (GE_EXPR, type, 2);
						    res_op->ops[0] = captures[0];
						    res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), max);
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 391, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail947:;
						}
					      else
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail948;
						  {
						    res_op->set_op (LE_EXPR, type, 2);
						    res_op->ops[0] = captures[0];
						    res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), max);
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 392, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail948:;
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
							  gimple_seq *lseq = seq;
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail949;
							  {
							    res_op->set_op (GE_EXPR, type, 2);
							    res_op->ops[0] = captures[0];
							    res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), max);
							    res_op->resimplify (lseq, valueize);
							    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 393, __FILE__, __LINE__, true);
							    return true;
							  }
next_after_fail949:;
							}
						      else
							{
							  gimple_seq *lseq = seq;
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail950;
							  {
							    res_op->set_op (LE_EXPR, type, 2);
							    res_op->ops[0] = captures[0];
							    res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), max);
							    res_op->resimplify (lseq, valueize);
							    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 394, __FILE__, __LINE__, true);
							    return true;
							  }
next_after_fail950:;
							}
						    }
						  else
						    {
						      if (neg
)
							{
							  gimple_seq *lseq = seq;
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail951;
							  {
							    res_op->set_op (UNGE_EXPR, type, 2);
							    res_op->ops[0] = captures[0];
							    res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), max);
							    res_op->resimplify (lseq, valueize);
							    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 395, __FILE__, __LINE__, true);
							    return true;
							  }
next_after_fail951:;
							}
						      else
							{
							  gimple_seq *lseq = seq;
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail952;
							  {
							    res_op->set_op (UNLE_EXPR, type, 2);
							    res_op->ops[0] = captures[0];
							    res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), max);
							    res_op->resimplify (lseq, valueize);
							    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 396, __FILE__, __LINE__, true);
							    return true;
							  }
next_after_fail952:;
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
  return false;
}

bool
gimple_simplify_282 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1016;
  {
    res_op->set_op (icmp, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[1]));
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 453, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1016:;
  return false;
}

bool
gimple_simplify_285 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (ncmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && types_match (captures[0], captures[2])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1019;
      {
	res_op->set_op (ncmp, type, 2);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] = captures[2];
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1019;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 455, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1019:;
    }
  return false;
}

bool
gimple_simplify_289 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 poly_int64 off0, off1;
 tree base0, base1;
 int equal = address_compare (cmp, TREE_TYPE (captures[0]), captures[1], captures[2], base0, base1,
 off0, off1,
0
);
      if (equal == 1
)
	{
	  if (cmp == EQ_EXPR && (known_eq (off0, off1) || known_ne (off0, off1))
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1023;
	      {
		tree tem;
		tem =  constant_boolean_node (known_eq (off0, off1), type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 458, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1023:;
	    }
	  else
	    {
	      if (cmp == NE_EXPR && (known_eq (off0, off1) || known_ne (off0, off1))
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1024;
		  {
		    tree tem;
		    tem =  constant_boolean_node (known_ne (off0, off1), type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 459, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1024:;
		}
	      else
		{
		  if (cmp == LT_EXPR && (known_lt (off0, off1) || known_ge (off0, off1))
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1025;
		      {
			tree tem;
			tem =  constant_boolean_node (known_lt (off0, off1), type);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 460, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1025:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR && (known_le (off0, off1) || known_gt (off0, off1))
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1026;
			  {
			    tree tem;
			    tem =  constant_boolean_node (known_le (off0, off1), type);
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 461, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1026:;
			}
		      else
			{
			  if (cmp == GE_EXPR && (known_ge (off0, off1) || known_lt (off0, off1))
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1027;
			      {
				tree tem;
				tem =  constant_boolean_node (known_ge (off0, off1), type);
				res_op->set_value (tem);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 462, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1027:;
			    }
			  else
			    {
			      if (cmp == GT_EXPR && (known_gt (off0, off1) || known_le (off0, off1))
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1028;
				  {
				    tree tem;
				    tem =  constant_boolean_node (known_gt (off0, off1), type);
				    res_op->set_value (tem);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 463, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1028:;
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1029;
		  {
		    tree tem;
		    tem =  constant_boolean_node (false, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 464, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1029:;
		}
	      else
		{
		  if (cmp == NE_EXPR
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1030;
		      {
			tree tem;
			tem =  constant_boolean_node (true, type);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 465, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1030:;
		    }
		}
	    }
	}
  }
  return false;
}

bool
gimple_simplify_300 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
		  gimple_seq *lseq = seq;
		  if (lseq
		      && (!single_use (captures[0])))
		    lseq = NULL;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1064;
		  {
		    tree tem;
		    tem =  constant_boolean_node (op == EQ_EXPR ? false : true, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 491, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1064:;
		}
	    }
	  else
	    {
	      if (zero_val < 0 || zero_val >= prec
)
		{
		  gimple_seq *lseq = seq;
		  if (lseq
		      && (!single_use (captures[0])))
		    lseq = NULL;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1065;
		  {
		    res_op->set_op (op, type, 2);
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      _o1[1] =  wide_int_to_tree (type0,
 wi::mask (tree_to_uhwi (captures[3]) + 1,
 false, prec));
		      gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1065;
		      res_op->ops[0] = _r1;
		    }
		    res_op->ops[1] =  wide_int_to_tree (type0,
 wi::shifted_mask (tree_to_uhwi (captures[3]), 1,
 false, prec));
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 492, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1065:;
		}
	    }
	}
  }
  return false;
}

bool
gimple_simplify_308 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!REAL_VALUE_NEGATIVE (TREE_REAL_CST (captures[1]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1076;
      {
	res_op->set_op (ABS_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 503, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1076:;
    }
  else
    {
      if (!direct_internal_fn_supported_p (IFN_COPYSIGN, type,
 OPTIMIZE_FOR_BOTH)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1077;
	  {
	    res_op->set_op (NEGATE_EXPR, type, 1);
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[0];
	      gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1077;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 504, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1077:;
	}
    }
  return false;
}

bool
gimple_simplify_317 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && ssa_name_has_boolean_range (captures[1])
 && !flag_non_call_exceptions
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1088;
      {
	tree tem;
	tem = captures[0];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 513, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1088:;
    }
  return false;
}

bool
gimple_simplify_321 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1092;
	      {
		res_op->set_op (div, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] =  wide_int_to_tree (type, mul);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 517, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1092:;
	    }
	  else
	    {
	      if (TYPE_UNSIGNED (type)
 || mul != wi::min_value (TYPE_PRECISION (type), SIGNED)
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1093;
		  {
		    tree tem;
		    tem =  build_zero_cst (type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 518, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1093:;
		}
	    }
	}
  }
  return false;
}

bool
gimple_simplify_327 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
int_range_max vr0, vr1, vr2, vr3, vr4;
      if (INTEGRAL_TYPE_P (type)
 && get_range_query (cfun)->range_of_expr (vr1, captures[3])
 && get_range_query (cfun)->range_of_expr (vr2, captures[4])
 && range_op_handler (MULT_EXPR).overflow_free_p (vr1, vr2)
 && get_range_query (cfun)->range_of_expr (vr0, captures[1])
 && get_range_query (cfun)->range_of_expr (vr3, captures[2])
 && range_op_handler (MINUS_EXPR).overflow_free_p (vr0, vr3)
 && get_range_query (cfun)->range_of_expr (vr4, captures[0])
 && !vr4.undefined_p ()
 && (TYPE_UNSIGNED (type)
 || (vr0.nonnegative_p () && vr4.nonnegative_p ())
 || (vr0.nonpositive_p () && vr4.nonpositive_p ()))
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1102;
	  {
	    res_op->set_op (MINUS_EXPR, type, 2);
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[1];
	      _o1[1] = captures[4];
	      gimple_match_op tem_op (res_op->cond.any_else (), div, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1102;
	      res_op->ops[0] = _r1;
	    }
	    res_op->ops[1] = captures[3];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 527, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1102:;
	}
  }
  return false;
}

bool
gimple_simplify_338 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SQRT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1115;
      {
	res_op->set_op (SQRT, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 540, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1115:;
    }
  return false;
}

bool
gimple_simplify_343 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COS),
 const combined_fn ARG_UNUSED (SIN),
 const combined_fn ARG_UNUSED (TAN))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[2])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1120;
      {
	res_op->set_op (RDIV_EXPR, type, 2);
	res_op->ops[0] =  build_one_cst (type);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  gimple_match_op tem_op (res_op->cond.any_else (), TAN, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1120;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 545, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1120:;
    }
  return false;
}

bool
gimple_simplify_348 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1126;
      {
	tree tem;
	tem =  constant_boolean_node (cmp == LT_EXPR, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 551, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1126:;
    }
  return false;
}

bool
gimple_simplify_352 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[1]))
 && (CONSTANT_CLASS_P (captures[1]) || single_use (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1131;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[1];
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[1]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 556, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1131:;
    }
  return false;
}

bool
gimple_simplify_357 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1137;
  {
    tree tem;
    tem =  constant_boolean_node (cmp == GE_EXPR || cmp == LE_EXPR, type);
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 562, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1137:;
  return false;
}

bool
gimple_simplify_362 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1144;
		  {
		    tree tem;
		    tem =  constant_boolean_node (false, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 569, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1144:;
		}
	      else
		{
		  if (cmp == GE_EXPR
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1145;
		      {
			res_op->set_op (EQ_EXPR, type, 2);
			res_op->ops[0] = captures[0];
			res_op->ops[1] = captures[2];
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 570, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1145:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1146;
			  {
			    tree tem;
			    tem =  constant_boolean_node (true, type);
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 571, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1146:;
			}
		      else
			{
			  if (cmp == LT_EXPR
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1147;
			      {
				res_op->set_op (NE_EXPR, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] = captures[2];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 572, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1147:;
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
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1148;
		      {
			tree tem;
			tem =  constant_boolean_node (false, type);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 573, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1148:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1149;
			  {
			    res_op->set_op (EQ_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] = captures[2];
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 574, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1149:;
			}
		      else
			{
			  if (cmp == GE_EXPR
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1150;
			      {
				tree tem;
				tem =  constant_boolean_node (true, type);
				res_op->set_value (tem);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 575, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1150:;
			    }
			  else
			    {
			      if (cmp == GT_EXPR
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1151;
				  {
				    res_op->set_op (NE_EXPR, type, 2);
				    res_op->ops[0] = captures[0];
				    res_op->ops[1] = captures[2];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 576, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1151:;
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
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1152;
			  {
			    res_op->set_op (EQ_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] =  build_uniform_cst (TREE_TYPE (captures[2]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 + 1));
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 577, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1152:;
			}
		      else
			{
			  if (cmp == LE_EXPR
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1153;
			      {
				res_op->set_op (NE_EXPR, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] =  build_uniform_cst (TREE_TYPE (captures[2]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 + 1));
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 578, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1153:;
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
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1154;
			      {
				res_op->set_op (NE_EXPR, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] =  build_uniform_cst (TREE_TYPE (captures[2]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 - 1));
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 579, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1154:;
			    }
			  else
			    {
			      if (cmp == LT_EXPR
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1155;
				  {
				    res_op->set_op (EQ_EXPR, type, 2);
				    res_op->ops[0] = captures[0];
				    res_op->ops[1] =  build_uniform_cst (TREE_TYPE (captures[2]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 - 1));
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 580, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1155:;
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
					  gimple_seq *lseq = seq;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1156;
					  {
					    res_op->set_op (GE_EXPR, type, 2);
					    {
					      tree _o1[1], _r1;
					      _o1[0] = captures[1];
					      if (st != TREE_TYPE (_o1[0])
					          && !useless_type_conversion_p (st, TREE_TYPE (_o1[0])))
						{
						  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, st, _o1[0]);
						  tem_op.resimplify (lseq, valueize);
						  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r1) goto next_after_fail1156;
					        }
					      else
					        _r1 = _o1[0];
					      res_op->ops[0] = _r1;
					    }
					    res_op->ops[1] =  build_zero_cst (st);
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 581, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail1156:;
					}
				      else
					{
					  if (cst == captures[2] && cmp == GT_EXPR
)
					    {
					      gimple_seq *lseq = seq;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1157;
					      {
						res_op->set_op (LT_EXPR, type, 2);
						{
						  tree _o1[1], _r1;
						  _o1[0] = captures[1];
						  if (st != TREE_TYPE (_o1[0])
						      && !useless_type_conversion_p (st, TREE_TYPE (_o1[0])))
						    {
						      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, st, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail1157;
						    }
						  else
						    _r1 = _o1[0];
						  res_op->ops[0] = _r1;
						}
						res_op->ops[1] =  build_zero_cst (st);
						res_op->resimplify (lseq, valueize);
						if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 582, __FILE__, __LINE__, true);
						return true;
					      }
next_after_fail1157:;
					    }
					  else
					    {
					      if (cmp == LE_EXPR
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1158;
						  {
						    res_op->set_op (GE_EXPR, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, st, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail1158;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  build_zero_cst (st);
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 583, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail1158:;
						}
					      else
						{
						  if (cmp == GT_EXPR
)
						    {
						      gimple_seq *lseq = seq;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1159;
						      {
							res_op->set_op (LT_EXPR, type, 2);
							{
							  tree _o1[1], _r1;
							  _o1[0] = captures[1];
							  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, st, _o1[0]);
							  tem_op.resimplify (lseq, valueize);
							  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
							  if (!_r1) goto next_after_fail1159;
							  res_op->ops[0] = _r1;
							}
							res_op->ops[1] =  build_zero_cst (st);
							res_op->resimplify (lseq, valueize);
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 584, __FILE__, __LINE__, true);
							return true;
						      }
next_after_fail1159:;
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
  return false;
}

bool
gimple_simplify_398 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((integer_zerop (captures[1]) || integer_onep (captures[1]))
)
    {
      if ((eqne == EQ_EXPR) ^ integer_zerop (captures[1])
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1229;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    res_op->ops[0] = captures[0];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 631, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1229:;
	}
      else
	{
	  if (types_match (type, TREE_TYPE (captures[0]))
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1230;
	      {
		res_op->set_op (BIT_XOR_EXPR, type, 2);
		res_op->ops[0] = captures[0];
		res_op->ops[1] =  build_one_cst (type);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 632, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1230:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_404 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1236;
	  {
	    res_op->set_op (op, type, 2);
	    res_op->ops[0] = captures[2];
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[4];
	      if (type != TREE_TYPE (_o1[0])
	          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
		{
		  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1236;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 638, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1236:;
	}
      else
	{
	  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1237;
	      {
		res_op->set_op (NOP_EXPR, type, 1);
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[2];
		    if (utype != TREE_TYPE (_o2[0])
		        && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
		      {
			gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1237;
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[4];
		    if (utype != TREE_TYPE (_o2[0])
		        && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
		      {
			gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1237;
		      }
		    else
		      _r2 = _o2[0];
		    _o1[1] = _r2;
		  }
		  gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1237;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 639, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1237:;
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
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[0])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1238;
			  {
			    res_op->set_op (op, type, 2);
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[2];
			      if (newtype != TREE_TYPE (_o1[0])
			          && !useless_type_conversion_p (newtype, TREE_TYPE (_o1[0])))
				{
				  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, newtype, _o1[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail1238;
			        }
			      else
			        _r1 = _o1[0];
			      res_op->ops[0] = _r1;
			    }
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[4];
			      if (newtype != TREE_TYPE (_o1[0])
			          && !useless_type_conversion_p (newtype, TREE_TYPE (_o1[0])))
				{
				  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, newtype, _o1[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail1238;
			        }
			      else
			        _r1 = _o1[0];
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 640, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1238:;
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
				  gimple_seq *lseq = seq;
				  if (lseq
				      && (!single_use (captures[0])))
				    lseq = NULL;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1239;
				  {
				    res_op->set_op (NOP_EXPR, type, 1);
				    {
				      tree _o1[2], _r1;
				      {
					tree _o2[1], _r2;
					_o2[0] = captures[2];
					if (newtype != TREE_TYPE (_o2[0])
					    && !useless_type_conversion_p (newtype, TREE_TYPE (_o2[0])))
					  {
					    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, newtype, _o2[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail1239;
					  }
					else
					  _r2 = _o2[0];
					_o1[0] = _r2;
				      }
				      {
					tree _o2[1], _r2;
					_o2[0] = captures[4];
					if (newtype != TREE_TYPE (_o2[0])
					    && !useless_type_conversion_p (newtype, TREE_TYPE (_o2[0])))
					  {
					    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, newtype, _o2[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail1239;
					  }
					else
					  _r2 = _o2[0];
					_o1[1] = _r2;
				      }
				      gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1239;
				      res_op->ops[0] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 641, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1239:;
				}
			  }
			}
		  }
		}
	  }
	}
    }
  return false;
}

bool
gimple_simplify_426 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
 && bitwise_inverted_equal_p (captures[0], captures[1], wascmp)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1272;
	  {
	    tree tem;
	    tem =  wascmp ? constant_boolean_node (false, type) : build_zero_cst (type);
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 662, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1272:;
	}
  }
  return false;
}

bool
gimple_simplify_433 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[3])
          || !single_use (captures[4])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1279;
  {
    res_op->set_op (BIT_AND_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    {
      tree _o1[1], _r1;
      _o1[0] = captures[5];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1279;
      res_op->ops[1] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 668, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1279:;
  return false;
}

bool
gimple_simplify_440 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1286;
  {
    res_op->set_op (BIT_XOR_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 675, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1286:;
  return false;
}

bool
gimple_simplify_442 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])
          || !single_use (captures[3])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1288;
  {
    res_op->set_op (BIT_NOT_EXPR, type, 1);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1288;
      res_op->ops[0] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 677, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1288:;
  return false;
}

bool
gimple_simplify_448 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])
	              || !single_use (captures[1])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1294;
	      {
		tree tem;
		tem =  build_int_cst (type, 0);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 682, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1294:;
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
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[0])
					          || !single_use (captures[1])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1295;
					  {
					    res_op->set_op (BIT_AND_EXPR, type, 2);
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[2];
						  if (shift_type != TREE_TYPE (_o3[0])
						      && !useless_type_conversion_p (shift_type, TREE_TYPE (_o3[0])))
						    {
						      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, shift_type, _o3[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r3) goto next_after_fail1295;
						    }
						  else
						    _r3 = _o3[0];
						  _o2[0] = _r3;
						}
						_o2[1] = captures[4];
						gimple_match_op tem_op (res_op->cond.any_else (), shift, shift_type, _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail1295;
						_o1[0] = _r2;
					      }
					      if (type != TREE_TYPE (_o1[0])
					          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
						{
						  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
						  tem_op.resimplify (lseq, valueize);
						  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r1) goto next_after_fail1295;
					        }
					      else
					        _r1 = _o1[0];
					      res_op->ops[0] = _r1;
					    }
					    res_op->ops[1] =  newmaskt;
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 683, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail1295:;
					}
				      else
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[0])
					          || !single_use (captures[1])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1296;
					  {
					    res_op->set_op (BIT_AND_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    res_op->ops[1] =  newmaskt;
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 684, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail1296:;
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
  return false;
}

bool
gimple_simplify_465 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1350;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 707, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1350:;
  return false;
}

bool
gimple_simplify_469 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])
	              || !single_use (captures[1])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1356;
	      {
		tree tem;
		tem =  build_int_cst (type, 0);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 682, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1356:;
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
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[0])
					          || !single_use (captures[1])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1357;
					  {
					    res_op->set_op (BIT_AND_EXPR, type, 2);
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[2];
						  if (shift_type != TREE_TYPE (_o3[0])
						      && !useless_type_conversion_p (shift_type, TREE_TYPE (_o3[0])))
						    {
						      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, shift_type, _o3[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r3) goto next_after_fail1357;
						    }
						  else
						    _r3 = _o3[0];
						  _o2[0] = _r3;
						}
						_o2[1] = captures[4];
						gimple_match_op tem_op (res_op->cond.any_else (), shift, shift_type, _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail1357;
						_o1[0] = _r2;
					      }
					      if (type != TREE_TYPE (_o1[0])
					          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
						{
						  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
						  tem_op.resimplify (lseq, valueize);
						  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r1) goto next_after_fail1357;
					        }
					      else
					        _r1 = _o1[0];
					      res_op->ops[0] = _r1;
					    }
					    res_op->ops[1] =  newmaskt;
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 683, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail1357:;
					}
				      else
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[0])
					          || !single_use (captures[1])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1358;
					  {
					    res_op->set_op (BIT_AND_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    res_op->ops[1] =  newmaskt;
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 684, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail1358:;
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
  return false;
}

bool
gimple_simplify_489 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1381;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[3];
		if (rotate_type != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (rotate_type, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, rotate_type, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail1381;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] = captures[4];
	      gimple_match_op tem_op (res_op->cond.any_else (), rotate, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1381;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 726, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1381:;
      }
    }
  return false;
}

bool
gimple_simplify_501 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (logic))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1406;
  {
    res_op->set_op (logic, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 739, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1406:;
  return false;
}

bool
gimple_simplify_505 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (real_isnan (TREE_REAL_CST_PTR (captures[1]))
 && (!HONOR_SNANS (captures[1]) || !TREE_REAL_CST (captures[1]).signalling)
 && !tree_expr_maybe_signaling_nan_p (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1410;
      {
	tree tem;
	tem = captures[0];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 743, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1410:;
    }
  return false;
}

bool
gimple_simplify_510 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1415;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 748, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1415:;
  return false;
}

bool
gimple_simplify_517 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[0], captures[3])
 && expr_no_side_effects_p (captures[2])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1422;
      {
	tree tem;
	tem = captures[1];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 755, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1422:;
    }
  return false;
}

bool
gimple_simplify_523 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGNED_ZEROS (type)
 && (
 (INTEGRAL_TYPE_P (type)
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[2]), TREE_TYPE (captures[3]))
 && TYPE_PRECISION (TREE_TYPE (captures[1])) <= TYPE_PRECISION (type)
 && TYPE_PRECISION (TREE_TYPE (captures[3])) <= TYPE_PRECISION (type))
 || (types_match (type, TREE_TYPE (captures[1]))
 && types_match (type, TREE_TYPE (captures[3]))))
)
    {
      if (cmp == EQ_EXPR
)
	{
	  if (VECTOR_TYPE_P (type)
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1453;
	      {
		res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
		res_op->ops[0] = captures[2];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 762, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1453:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1454;
	      {
		res_op->set_op (NOP_EXPR, type, 1);
		res_op->ops[0] = captures[2];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 763, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1454:;
	    }
	}
      else
	{
	  if (cmp == NE_EXPR
)
	    {
	      if (VECTOR_TYPE_P (type)
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1455;
		  {
		    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
		    res_op->ops[0] = captures[0];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 764, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1455:;
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1456;
		  {
		    res_op->set_op (NOP_EXPR, type, 1);
		    res_op->ops[0] = captures[0];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 765, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1456:;
		}
	    }
	  else
	    {
	      if (cmp == LE_EXPR || cmp == UNLE_EXPR || cmp == LT_EXPR || cmp == UNLT_EXPR
)
		{
		  if (!HONOR_NANS (type)
)
		    {
		      if (VECTOR_TYPE_P (type)
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1457;
			  {
			    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[2];
			      gimple_match_op tem_op (res_op->cond.any_else (), MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1457;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 766, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1457:;
			}
		      else
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1458;
			  {
			    res_op->set_op (NOP_EXPR, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[2];
			      gimple_match_op tem_op (res_op->cond.any_else (), MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1458;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 767, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1458:;
			}
		    }
		}
	      else
		{
		  if (cmp == GE_EXPR || cmp == UNGE_EXPR || cmp == GT_EXPR || cmp == UNGT_EXPR
)
		    {
		      if (!HONOR_NANS (type)
)
			{
			  if (VECTOR_TYPE_P (type)
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1459;
			      {
				res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[0];
				  _o1[1] = captures[2];
				  gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail1459;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 768, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1459:;
			    }
			  else
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1460;
			      {
				res_op->set_op (NOP_EXPR, type, 1);
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[0];
				  _o1[1] = captures[2];
				  gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail1460;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 769, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1460:;
			    }
			}
		    }
		  else
		    {
		      if (cmp == UNEQ_EXPR
)
			{
			  if (!HONOR_NANS (type)
)
			    {
			      if (VECTOR_TYPE_P (type)
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1461;
				  {
				    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				    res_op->ops[0] = captures[2];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 770, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1461:;
				}
			      else
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1462;
				  {
				    res_op->set_op (NOP_EXPR, type, 1);
				    res_op->ops[0] = captures[2];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 771, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1462:;
				}
			    }
			}
		      else
			{
			  if (cmp == LTGT_EXPR
)
			    {
			      if (!HONOR_NANS (type)
)
				{
				  if (VECTOR_TYPE_P (type)
)
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1463;
				      {
					res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
					res_op->ops[0] = captures[0];
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 772, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1463:;
				    }
				  else
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1464;
				      {
					res_op->set_op (NOP_EXPR, type, 1);
					res_op->ops[0] = captures[0];
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 773, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1464:;
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
  return false;
}

bool
gimple_simplify_549 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1506;
  {
    tree tem;
    tem = captures[3];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 795, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1506:;
  return false;
}

bool
gimple_simplify_556 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1513;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 799, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1513:;
  return false;
}

bool
gimple_simplify_563 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGNED_ZEROS (type)
 && !TYPE_UNSIGNED (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1525;
      {
	res_op->set_op (ABS_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 805, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1525:;
    }
  return false;
}

bool
gimple_simplify_568 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGNED_ZEROS (type)
 && !TYPE_UNSIGNED (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1536;
      {
	res_op->set_op (ABS_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 805, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1536:;
    }
  return false;
}

bool
gimple_simplify_569 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1537;
		  {
		    res_op->set_op (NOP_EXPR, type, 1);
		    {
		      tree _o1[1], _r1;
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[0];
			gimple_match_op tem_op (res_op->cond.any_else (), ABSU_EXPR, utype, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1537;
			_o1[0] = _r2;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1537;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 810, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1537:;
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1538;
		  {
		    res_op->set_op (NOP_EXPR, type, 1);
		    {
		      tree _o1[1], _r1;
		      {
			tree _o2[1], _r2;
			{
			  tree _o3[1], _r3;
			  _o3[0] = captures[2];
			  if (stype != TREE_TYPE (_o3[0])
			      && !useless_type_conversion_p (stype, TREE_TYPE (_o3[0])))
			    {
			      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, stype, _o3[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r3) goto next_after_fail1538;
			    }
			  else
			    _r3 = _o3[0];
			  _o2[0] = _r3;
			}
			gimple_match_op tem_op (res_op->cond.any_else (), ABSU_EXPR, utype, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1538;
			_o1[0] = _r2;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1538;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 811, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1538:;
		}
	  }
	}
      else
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1539;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[1], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[2];
		gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1539;
		_o1[0] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1539;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 812, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1539:;
	}
    }
  return false;
}

bool
gimple_simplify_583 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 int val;
 internal_fn ifn = IFN_LAST;
 if (TREE_CODE (TREE_TYPE (captures[2])) == BITINT_TYPE)
 ifn = IFN_CLZ;
 else if (direct_internal_fn_supported_p (IFN_CLZ, TREE_TYPE (captures[2]),
 OPTIMIZE_FOR_BOTH))
 ifn = IFN_CLZ;
      if (ifn == IFN_CLZ
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1563;
	  {
	    res_op->set_op (CFN_CLZ, type, 2);
	    res_op->ops[0] = captures[2];
	    res_op->ops[1] = captures[3];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 822, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1563:;
	}
  }
  return false;
}

bool
gimple_simplify_589 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1569;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[1];
	      {
		tree _o2[2], _r2;
		_o2[0] =  TYPE_MAX_VALUE (TREE_TYPE (captures[1]));
		_o2[1] = captures[2];
		gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_DIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		tem_op.resimplify (NULL, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, NULL);
		if (!_r2) goto next_after_fail1569;
		_o1[1] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, boolean_type_node, _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1569;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 826, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1569:;
	}
      else
	{
	  if (TYPE_MIN_VALUE (TREE_TYPE (captures[1]))
)
	    {
	      if (integer_minus_onep (captures[2])
)
		{
		  gimple_seq *lseq = seq;
		  if (lseq
		      && (!single_use (captures[0])))
		    lseq = NULL;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1570;
		  {
		    res_op->set_op (NOP_EXPR, type, 1);
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      _o1[1] =  TYPE_MIN_VALUE (TREE_TYPE (captures[1]));
		      gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, boolean_type_node, _o1[0], _o1[1]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1570;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 827, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1570:;
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
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[0])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1571;
			  {
			    res_op->set_op (NOP_EXPR, type, 1);
			    {
			      tree _o1[2], _r1;
			      {
				tree _o2[2], _r2;
				{
				  tree _o3[1], _r3;
				  _o3[0] = captures[1];
				  if (etype != TREE_TYPE (_o3[0])
				      && !useless_type_conversion_p (etype, TREE_TYPE (_o3[0])))
				    {
				      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, etype, _o3[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r3) goto next_after_fail1571;
				    }
				  else
				    _r3 = _o3[0];
				  _o2[0] = _r3;
				}
				_o2[1] =  lo;
				gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail1571;
				_o1[0] = _r2;
			      }
			      _o1[1] =  hi;
			      gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, boolean_type_node, _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1571;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 828, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1571:;
			}
		  }
		}
	    }
	}
    }
  return false;
}

bool
gimple_simplify_617 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ovf))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) > TYPE_PRECISION (TREE_TYPE (captures[1]))
 && (!TYPE_UNSIGNED (TREE_TYPE (captures[0])) || TYPE_UNSIGNED (TREE_TYPE (captures[1])))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1599;
      {
	res_op->set_op (ovf, type, 2);
	res_op->ops[0] = captures[1];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 851, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1599:;
    }
  return false;
}

bool
gimple_simplify_626 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1608;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 860, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1608:;
  return false;
}

bool
gimple_simplify_631 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1613;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 865, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1613:;
  return false;
}

bool
gimple_simplify_636 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ifn),
 const combined_fn ARG_UNUSED (lfn),
 const combined_fn ARG_UNUSED (llfn))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_p ()
)
    {
      if (TYPE_PRECISION (integer_type_node)
 == TYPE_PRECISION (long_integer_type_node)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1618;
	  {
	    res_op->set_op (lfn, type, 1);
	    res_op->ops[0] = captures[0];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1618:;
	}
    }
  return false;
}

bool
gimple_simplify_643 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fmas))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1625;
      {
	res_op->set_op (CFN_FMS, type, 3);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[1];
	res_op->ops[2] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 877, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1625:;
    }
  return false;
}

bool
gimple_simplify_651 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree t = TREE_TYPE (captures[0]);
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1633;
      {
	res_op->set_op (ctz, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  if (t != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (t, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, t, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1633;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 884, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1633:;
  }
  return false;
}

bool
gimple_simplify_659 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rot),
 const combined_fn ARG_UNUSED (popcount))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && (
1
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
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])
	              || !single_use (captures[1])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1645;
	      {
		res_op->set_op (popcount, type, 1);
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[2];
		  if (type0 != TREE_TYPE (_o1[0])
		      && !useless_type_conversion_p (type0, TREE_TYPE (_o1[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type0, _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1645;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 889, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1645:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_665 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1654;
	  {
	    res_op->set_op (parity, type, 1);
	    {
	      tree _o1[1], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[2];
		if (type1 != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (type1, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type1, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail1654;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      if (type0 != TREE_TYPE (_o1[0])
	          && !useless_type_conversion_p (type0, TREE_TYPE (_o1[0])))
		{
		  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type0, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1654;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 896, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1654:;
      }
    }
  return false;
}

bool
gimple_simplify_673 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (plusminus),
 const enum tree_code ARG_UNUSED (minusplus))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!VECTOR_INTEGER_TYPE_P (type)
 && !FLOAT_WORDS_BIG_ENDIAN
 && (operand_equal_p (captures[0], captures[2], 0)
 ? operand_equal_p (captures[1], captures[3], 0)
 : operand_equal_p (captures[0], captures[3], 0) && operand_equal_p (captures[1], captures[2], 0))
)
    {
      {
 vec_perm_builder builder;
	  if (tree_to_vec_perm_builder (&builder, captures[4])
)
	    {
	      {
 poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (type);
 vec_perm_indices sel (builder, 2, nelts);
 machine_mode vec_mode = TYPE_MODE (type);
 machine_mode wide_mode;
 scalar_mode wide_elt_mode;
 poly_uint64 wide_nunits;
 scalar_mode inner_mode = GET_MODE_INNER (vec_mode);
		  if (VECTOR_MODE_P (vec_mode)
 && sel.series_p (0, 2, 0, 2)
 && sel.series_p (1, 2, nelts + 1, 2)
 && GET_MODE_2XWIDER_MODE (inner_mode).exists (&wide_elt_mode)
 && multiple_p (GET_MODE_NUNITS (vec_mode), 2, &wide_nunits)
 && related_vector_mode (vec_mode, wide_elt_mode,
 wide_nunits).exists (&wide_mode)
)
		    {
		      {
 tree stype
 = lang_hooks.types.type_for_mode (GET_MODE_INNER (wide_mode),
 TYPE_UNSIGNED (type));
 tree ntype = build_vector_type_for_mode (stype, wide_mode);
 const struct real_format *fmt_old = FLOAT_MODE_FORMAT (vec_mode);
 const struct real_format *fmt_new = FLOAT_MODE_FORMAT (wide_mode);
			  if (TYPE_MODE (stype) != BLKmode
 && VECTOR_TYPE_P (ntype)
 && fmt_old != NULL
 && fmt_new != NULL
)
			    {
			      {
 if (known_eq (GET_MODE_NUNITS (wide_mode), 1)
 && !target_supports_op_p (ntype, NEGATE_EXPR, optab_vector))
 ntype = stype;
				  if (fmt_new->signbit_rw
 == fmt_old->signbit_rw + GET_MODE_UNIT_BITSIZE (vec_mode)
 && fmt_new->signbit_rw == fmt_new->signbit_ro
 && targetm.can_change_mode_class (TYPE_MODE (ntype),
 TYPE_MODE (type), ALL_REGS)
 && ((optimize_vectors_before_lowering_p ()
 && VECTOR_TYPE_P (ntype))
 || target_supports_op_p (ntype, NEGATE_EXPR, optab_vector))
)
				    {
				      if (plusminus == PLUS_EXPR
)
					{
					  gimple_seq *lseq = seq;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1664;
					  {
					    res_op->set_op (PLUS_EXPR, type, 2);
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[1], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[3];
						  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, ntype, _o3[0]);
						  tem_op.resimplify (lseq, valueize);
						  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r3) goto next_after_fail1664;
						  _o2[0] = _r3;
						}
						gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail1664;
						_o1[0] = _r2;
					      }
					      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail1664;
					      res_op->ops[0] = _r1;
					    }
					    res_op->ops[1] = captures[2];
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 909, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail1664:;
					}
				      else
					{
					  gimple_seq *lseq = seq;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1665;
					  {
					    res_op->set_op (MINUS_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[1], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, ntype, _o3[0]);
						  tem_op.resimplify (lseq, valueize);
						  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r3) goto next_after_fail1665;
						  _o2[0] = _r3;
						}
						gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail1665;
						_o1[0] = _r2;
					      }
					      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail1665;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 910, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail1665:;
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COSH (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    const combined_fn coss = CFN_BUILT_IN_COSH;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1704;
		    {
		      res_op->set_op (coss, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 932, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1704:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    const combined_fn coss = CFN_BUILT_IN_COSH;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1705;
		    {
		      res_op->set_op (coss, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 932, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1705:;
		  }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
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
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_BUILT_IN_COSH))
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
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_BUILT_IN_COSH))
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
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_BUILT_IN_COSH))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_ATANH:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		      if (gimple_simplify_411 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COSH, CFN_BUILT_IN_ATANH, CFN_BUILT_IN_SQRT))
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
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_BUILT_IN_COSH))
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_CCOSHL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1728;
		    {
		      res_op->set_op (CFN_BUILT_IN_CCOSHL, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 944, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1728:;
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
gimple_simplify_VIEW_CONVERT_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if ((
1
 && useless_type_conversion_p (type, TREE_TYPE (captures[0])))
 || (
0
 && type == TREE_TYPE (captures[0]))
)
      {
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1756;
	{
	  tree tem;
	  tem = captures[0];
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 921, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail1756:;
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
	      case VIEW_CONVERT_EXPR:
	        {
		  tree _q20 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q20) == SSA_NAME
		       || is_gimple_min_invariant (_q20)))
		    {
		      _q20 = do_valueize (valueize, _q20);
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1757;
			{
			  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			  res_op->ops[0] = captures[0];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 958, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail1757:;
		      }
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
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if ((INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type))
 && (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0])))
 && TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (captures[0]))
)
      {
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1758;
	{
	  res_op->set_op (NOP_EXPR, type, 1);
	  res_op->ops[0] = captures[0];
	  res_op->resimplify (lseq, valueize);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 959, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail1758:;
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
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1759;
			{
			  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			  res_op->ops[0] = captures[1];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 960, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail1759:;
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
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1760;
			    {
			      tree tem;
			      tem =  build_zero_cst (type);
			      res_op->set_value (tem);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 961, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail1760:;
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
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1761;
				{
				  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				  res_op->ops[0] =  CONSTRUCTOR_ELT (ctor, 0)->value;
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 962, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail1761:;
			      }
			  }
		    }
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
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		    if (VECTOR_TYPE_P (type) && VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 && known_eq (TYPE_VECTOR_SUBPARTS (type),
 TYPE_VECTOR_SUBPARTS (TREE_TYPE (captures[2])))
 && tree_nop_conversion_p (TREE_TYPE (type), TREE_TYPE (TREE_TYPE (captures[2])))
)
		      {
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[0])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1762;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[1], _r1;
			    _o1[0] = captures[2];
			    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail1762;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[1], _r1;
			    _o1[0] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail1762;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 963, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail1762:;
		      }
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
gimple_simplify_CONJ_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case CONJ_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  {
				    tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
				    if (gimple_simplify_590 (res_op, seq, valueize, type, captures))
				      return true;
				  }
			          break;
			        }
			      case COMPLEX_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
				    {
 tree itype = TREE_TYPE (type);
					gimple_seq *lseq = seq;
					if (lseq
					    && (!single_use (captures[0])
					        || !single_use (captures[1])))
					  lseq = NULL;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1822;
					{
					  res_op->set_op (COMPLEX_EXPR, type, 2);
					  {
					    tree _o1[1], _r1;
					    _o1[0] = captures[2];
					    if (itype != TREE_TYPE (_o1[0])
					        && !useless_type_conversion_p (itype, TREE_TYPE (_o1[0])))
					      {
						gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, itype, _o1[0]);
						tem_op.resimplify (lseq, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r1) goto next_after_fail1822;
					      }
					    else
					      _r1 = _o1[0];
					    res_op->ops[0] = _r1;
					  }
					  {
					    tree _o1[1], _r1;
					    {
					      tree _o2[1], _r2;
					      _o2[0] = captures[3];
					      if (itype != TREE_TYPE (_o2[0])
					          && !useless_type_conversion_p (itype, TREE_TYPE (_o2[0])))
						{
						  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, itype, _o2[0]);
						  tem_op.resimplify (lseq, valueize);
						  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r2) goto next_after_fail1822;
					        }
					      else
					        _r2 = _o2[0];
					      _o1[0] = _r2;
					    }
					    gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail1822;
					    res_op->ops[1] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 974, __FILE__, __LINE__, true);
					  return true;
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
		    default:;
		    }
	          break;
	        }
	      case CONJ_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    if (gimple_simplify_590 (res_op, seq, valueize, type, captures))
		      return true;
		  }
	          break;
	        }
	      case COMPLEX_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
		    {
 tree itype = TREE_TYPE (type);
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[0])
			        || !single_use (captures[1])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1823;
			{
			  res_op->set_op (COMPLEX_EXPR, type, 2);
			  {
			    tree _o1[1], _r1;
			    _o1[0] = captures[2];
			    if (itype != TREE_TYPE (_o1[0])
			        && !useless_type_conversion_p (itype, TREE_TYPE (_o1[0])))
			      {
				gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, itype, _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail1823;
			      }
			    else
			      _r1 = _o1[0];
			    res_op->ops[0] = _r1;
			  }
			  {
			    tree _o1[1], _r1;
			    {
			      tree _o2[1], _r2;
			      _o2[0] = captures[3];
			      if (itype != TREE_TYPE (_o2[0])
			          && !useless_type_conversion_p (itype, TREE_TYPE (_o2[0])))
				{
				  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, itype, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail1823;
			        }
			      else
			        _r2 = _o2[0];
			      _o1[0] = _r2;
			    }
			    gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r1) goto next_after_fail1823;
			    res_op->ops[1] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 974, __FILE__, __LINE__, true);
			  return true;
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
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_BSWAP16 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
	      case BIT_NOT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_BSWAP16:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
				      if (gimple_simplify_592 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16))
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_BSWAP16:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR, CFN_BUILT_IN_BSWAP16))
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
		  switch (TREE_CODE (_q21))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_BSWAP16:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q40 = gimple_call_arg (_c2, 0);
				    _q40 = do_valueize (valueize, _q40);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR, CFN_BUILT_IN_BSWAP16))
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_BSWAP16:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, CFN_BUILT_IN_BSWAP16))
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
		  switch (TREE_CODE (_q21))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_BSWAP16:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q40 = gimple_call_arg (_c2, 0);
				    _q40 = do_valueize (valueize, _q40);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, CFN_BUILT_IN_BSWAP16))
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_BSWAP16:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, CFN_BUILT_IN_BSWAP16))
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
		  switch (TREE_CODE (_q21))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_BSWAP16:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q40 = gimple_call_arg (_c2, 0);
				    _q40 = do_valueize (valueize, _q40);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, CFN_BUILT_IN_BSWAP16))
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
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_BSWAP16:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_591 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16))
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LOGF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
	      case RDIV_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q20))
		    {
		    case REAL_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			  if (gimple_simplify_620 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOGF))
			    return true;
			}
		        break;
		      }
		    default:;
		    }
		if (real_onep (_q20))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		      if (gimple_simplify_619 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOGF))
		        return true;
		    }
		  }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_SQRTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_SQRTF)
 {
 CASE_CFN_SQRT:
 x = build_real (type, dconsthalf);
 break;
 CASE_CFN_CBRT:
 x = build_real_truncate (type, dconst_third ());
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1824;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGF, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1824;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1824:;
			      }
			    }
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP10F:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_EXP10F)
 {
 CASE_CFN_EXP:
 x = build_real_truncate (type, dconst_e ());
 break;
 CASE_CFN_EXP2:
 x = build_real (type, dconst2);
 break;
 CASE_CFN_EXP10:
 CASE_CFN_POW10:
 {
 REAL_VALUE_TYPE dconst10;
 real_from_integer (&dconst10, VOIDmode, 10, SIGNED);
 x = build_real (type, dconst10);
 }
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1825;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGF, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1825;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1825:;
			      }
			    }
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXPF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1826;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 977, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1826:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POWF:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		      if (gimple_simplify_621 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOGF, CFN_BUILT_IN_POWF))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POW10F:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_POW10F)
 {
 CASE_CFN_EXP:
 x = build_real_truncate (type, dconst_e ());
 break;
 CASE_CFN_EXP2:
 x = build_real (type, dconst2);
 break;
 CASE_CFN_EXP10:
 CASE_CFN_POW10:
 {
 REAL_VALUE_TYPE dconst10;
 real_from_integer (&dconst10, VOIDmode, 10, SIGNED);
 x = build_real (type, dconst10);
 }
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1827;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGF, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1827;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1827:;
			      }
			    }
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CBRTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_CBRTF)
 {
 CASE_CFN_SQRT:
 x = build_real (type, dconsthalf);
 break;
 CASE_CFN_CBRT:
 x = build_real_truncate (type, dconst_third ());
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1828;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGF, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1828;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1828:;
			      }
			    }
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP2F:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_EXP2F)
 {
 CASE_CFN_EXP:
 x = build_real_truncate (type, dconst_e ());
 break;
 CASE_CFN_EXP2:
 x = build_real (type, dconst2);
 break;
 CASE_CFN_EXP10:
 CASE_CFN_POW10:
 {
 REAL_VALUE_TYPE dconst10;
 real_from_integer (&dconst10, VOIDmode, 10, SIGNED);
 x = build_real (type, dconst10);
 }
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1829;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGF, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1829;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1829:;
			      }
			    }
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LOG2L (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
	      case RDIV_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q20))
		    {
		    case REAL_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			  if (gimple_simplify_620 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG2L))
			    return true;
			}
		        break;
		      }
		    default:;
		    }
		if (real_onep (_q20))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		      if (gimple_simplify_619 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG2L))
		        return true;
		    }
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
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_SQRTL)
 {
 CASE_CFN_SQRT:
 x = build_real (type, dconsthalf);
 break;
 CASE_CFN_CBRT:
 x = build_real_truncate (type, dconst_third ());
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1858;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOG2L, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1858;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1858:;
			      }
			    }
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
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_EXP10L)
 {
 CASE_CFN_EXP:
 x = build_real_truncate (type, dconst_e ());
 break;
 CASE_CFN_EXP2:
 x = build_real (type, dconst2);
 break;
 CASE_CFN_EXP10:
 CASE_CFN_POW10:
 {
 REAL_VALUE_TYPE dconst10;
 real_from_integer (&dconst10, VOIDmode, 10, SIGNED);
 x = build_real (type, dconst10);
 }
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1859;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOG2L, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1859;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1859:;
			      }
			    }
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
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_EXPL)
 {
 CASE_CFN_EXP:
 x = build_real_truncate (type, dconst_e ());
 break;
 CASE_CFN_EXP2:
 x = build_real (type, dconst2);
 break;
 CASE_CFN_EXP10:
 CASE_CFN_POW10:
 {
 REAL_VALUE_TYPE dconst10;
 real_from_integer (&dconst10, VOIDmode, 10, SIGNED);
 x = build_real (type, dconst10);
 }
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1860;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOG2L, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1860;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1860:;
			      }
			    }
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
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		      if (gimple_simplify_621 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG2L, CFN_BUILT_IN_POWL))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POW10L:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_POW10L)
 {
 CASE_CFN_EXP:
 x = build_real_truncate (type, dconst_e ());
 break;
 CASE_CFN_EXP2:
 x = build_real (type, dconst2);
 break;
 CASE_CFN_EXP10:
 CASE_CFN_POW10:
 {
 REAL_VALUE_TYPE dconst10;
 real_from_integer (&dconst10, VOIDmode, 10, SIGNED);
 x = build_real (type, dconst10);
 }
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1861;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOG2L, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1861;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1861:;
			      }
			    }
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CBRTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_CBRTL)
 {
 CASE_CFN_SQRT:
 x = build_real (type, dconsthalf);
 break;
 CASE_CFN_CBRT:
 x = build_real_truncate (type, dconst_third ());
 break;
 default:
 gcc_unreachable ();
 }
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1862;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOG2L, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1862;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1862:;
			      }
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
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1863;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 977, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1863:;
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_EXPL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_LOGL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1892;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1892:;
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_EXP2L (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_LOG2L:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1896;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1896:;
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
  return false;
}

bool
gimple_simplify_CFN_EXP10 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_LOG10:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1901;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1901:;
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
  return false;
}

bool
gimple_simplify_CFN_SQRT (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_EXP:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1926;
			  {
			    res_op->set_op (CFN_EXP, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real (type, dconsthalf);
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1926;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 980, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1926:;
			}
		    }
	          }
	        break;
	      case CFN_POW:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		      if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1927;
			  {
			    res_op->set_op (CFN_POW, type, 2);
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[0];
			      gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1927;
			      res_op->ops[0] = _r1;
			    }
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[1];
			      _o1[1] =  build_real (type, dconsthalf);
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1927;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 981, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1927:;
			}
		    }
	          }
	        break;
	      case CFN_EXP2:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1928;
			  {
			    res_op->set_op (CFN_EXP2, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real (type, dconsthalf);
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1928;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 980, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1928:;
			}
		    }
	          }
	        break;
	      case CFN_SQRT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1929;
			  {
			    res_op->set_op (CFN_POW, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] =  build_real (type, dconst_quarter ());
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 979, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1929:;
			}
		    }
	          }
	        break;
	      case CFN_EXP10:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1930;
			  {
			    res_op->set_op (CFN_EXP10, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real (type, dconsthalf);
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1930;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 980, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1930:;
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_TANF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_ATANF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_622 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TANF, CFN_BUILT_IN_ATANF))
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_TAN (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_ATAN:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_622 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TAN, CFN_BUILT_IN_ATAN))
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_SIN (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_ATAN:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		      if (gimple_simplify_623 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SIN, CFN_BUILT_IN_ATAN, CFN_BUILT_IN_SQRT, CFN_BUILT_IN_COPYSIGN))
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_TRUNCF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_TRUNCF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCF))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCF))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_TRUNCF32X (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_TRUNCF32X:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCF32X))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCF32X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FLOORF16 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_FLOORF16:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF16))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF16))
        return true;
    }
  }
if (tree_expr_nonnegative_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_629 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF16, CFN_BUILT_IN_TRUNCF16))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FLOORF64X (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_FLOORF64X:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF64X))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF64X))
        return true;
    }
  }
if (tree_expr_nonnegative_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_629 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF64X, CFN_BUILT_IN_TRUNCF64X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_CEILF32 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_CEILF32:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILF32))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILF32))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_ROUNDF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_ROUNDF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDF))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDF))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_ROUNDF32X (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_ROUNDF32X:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDF32X))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDF32X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_NEARBYINTF16 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_NEARBYINTF16:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINTF16))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINTF16))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_NEARBYINTF64X (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_NEARBYINTF64X:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINTF64X))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINTF64X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_RINTF32 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_RINTF32:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTF32))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTF32))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_RINT (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_RINT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_RINT))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_RINT))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_ICEILL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		  if (gimple_double_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_632 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ICEILL, CFN_BUILT_IN_ICEIL))
			  return true;
		      }
		    }
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ICEILL, CFN_BUILT_IN_ICEILF))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ICEILL))
        return true;
    }
  }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_636 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ICEILL, CFN_BUILT_IN_LCEILL, CFN_BUILT_IN_LLCEILL))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_IROUNDL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		  if (gimple_double_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_632 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IROUNDL, CFN_BUILT_IN_IROUND))
			  return true;
		      }
		    }
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IROUNDL, CFN_BUILT_IN_IROUNDF))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IROUNDL))
        return true;
    }
  }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_636 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IROUNDL, CFN_BUILT_IN_LROUNDL, CFN_BUILT_IN_LLROUNDL))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LFLOOR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LFLOOR, CFN_BUILT_IN_LFLOORF))
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
if (tree_expr_nonnegative_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_634 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LFLOOR))
        return true;
    }
  }
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LFLOOR))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LLROUND (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LLROUND, CFN_BUILT_IN_LLROUNDF))
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
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LLROUND))
        return true;
    }
  }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_637 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IROUND, CFN_BUILT_IN_LROUND, CFN_BUILT_IN_LLROUND))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_LFLOOR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_expr_nonnegative_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_634 (res_op, seq, valueize, type, captures, CFN_LFLOOR))
	  return true;
      }
    }
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_LFLOOR))
	  return true;
      }
    }
  return false;
}

bool
gimple_simplify_CFN_LCEIL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_LCEIL))
	  return true;
      }
    }
  return false;
}

bool
gimple_simplify_CFN_LROUND (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_LROUND))
	  return true;
      }
    }
  return false;
}

bool
gimple_simplify_CFN_IRINT (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_638 (res_op, seq, valueize, type, captures, CFN_IRINT))
	  return true;
      }
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_636 (res_op, seq, valueize, type, captures, CFN_IRINT, CFN_LRINT, CFN_LLRINT))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_SIGNBITF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_expr_nonnegative_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_639 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SIGNBITF))
	  return true;
      }
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_640 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SIGNBITF))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_CTZ (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    if (gimple_simplify_652 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZ))
		      return true;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    if (gimple_simplify_653 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZ))
		      return true;
		  }
	          break;
	        }
	      CASE_CONVERT:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    if (gimple_simplify_654 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZ))
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
		  case NEGATE_EXPR:
		    {
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_650 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZ))
			  return true;
		      }
		      break;
		    }
		  case ABS_EXPR:
		    {
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_651 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZ))
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
}
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_POPCOUNTLL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case LROTATE_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
				    if (gimple_simplify_656 (res_op, seq, valueize, type, captures, LROTATE_EXPR, CFN_BUILT_IN_POPCOUNTLL))
				      return true;
				  }
			          break;
			        }
			      case RROTATE_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
				    if (gimple_simplify_656 (res_op, seq, valueize, type, captures, RROTATE_EXPR, CFN_BUILT_IN_POPCOUNTLL))
				      return true;
				  }
			          break;
			        }
			      default:;
			      }
			  else if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_BSWAP128:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
				      if (gimple_simplify_655 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128, CFN_BUILT_IN_POPCOUNTLL))
				        return true;
				    }
			          }
			        break;
			      case CFN_BUILT_IN_BSWAP16:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
				      if (gimple_simplify_655 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16, CFN_BUILT_IN_POPCOUNTLL))
				        return true;
				    }
			          }
			        break;
			      case CFN_BUILT_IN_BSWAP32:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
				      if (gimple_simplify_655 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32, CFN_BUILT_IN_POPCOUNTLL))
				        return true;
				    }
			          }
			        break;
			      case CFN_BUILT_IN_BSWAP64:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
				      if (gimple_simplify_655 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64, CFN_BUILT_IN_POPCOUNTLL))
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
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    if (gimple_simplify_657 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POPCOUNTLL))
		      return true;
		  }
	          break;
	        }
	      case LROTATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
		    if (gimple_simplify_659 (res_op, seq, valueize, type, captures, LROTATE_EXPR, CFN_BUILT_IN_POPCOUNTLL))
		      return true;
		  }
	          break;
	        }
	      case RROTATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
		    if (gimple_simplify_659 (res_op, seq, valueize, type, captures, RROTATE_EXPR, CFN_BUILT_IN_POPCOUNTLL))
		      return true;
		  }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_BSWAP128:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		      if (gimple_simplify_658 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128, CFN_BUILT_IN_POPCOUNTLL))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP16:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		      if (gimple_simplify_658 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16, CFN_BUILT_IN_POPCOUNTLL))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP32:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		      if (gimple_simplify_658 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32, CFN_BUILT_IN_POPCOUNTLL))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP64:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		      if (gimple_simplify_658 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64, CFN_BUILT_IN_POPCOUNTLL))
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
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_660 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POPCOUNTLL))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FFS (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    if (gimple_simplify_668 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FFS))
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
if (tree_expr_nonzero_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_667 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FFS, CFN_BUILT_IN_CTZ))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_FFS (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    if (gimple_simplify_668 (res_op, seq, valueize, type, captures, CFN_FFS))
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
if (tree_expr_nonzero_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_667 (res_op, seq, valueize, type, captures, CFN_FFS, CFN_CTZ))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_REDUC_MIN (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
	      case MIN_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  switch (TREE_CODE (_q21))
		    {
		    case VECTOR_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1968;
			  {
			    res_op->set_op (MIN_EXPR, type, 2);
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[0];
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_MIN, type, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1968;
			      res_op->ops[0] = _r1;
			    }
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_MIN, type, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1968;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 991, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1968:;
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
gimple_simplify_CFN_REDUC_AND (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
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
		    case VECTOR_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1971;
			  {
			    res_op->set_op (BIT_AND_EXPR, type, 2);
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[0];
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_AND, type, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1971;
			      res_op->ops[0] = _r1;
			    }
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_AND, type, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1971;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 991, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1971:;
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
gimple_simplify_POINTER_DIFF_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2132;
	{
	  res_op->set_op (NOP_EXPR, type, 1);
	  res_op->ops[0] = captures[0];
	  res_op->resimplify (lseq, valueize);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1048, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail2132:;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _p0, 0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p0 };
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2133;
	{
	  tree tem;
	  tem =  build_zero_cst (type);
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1049, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail2133:;
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
	      case POINTER_PLUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case POINTER_PLUS_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1]))
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2134;
					    {
					      res_op->set_op (NOP_EXPR, type, 1);
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[1];
						_o1[1] = captures[2];
						gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (lseq, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r1) goto next_after_fail2134;
						res_op->ops[0] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1050, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2134:;
					  }
				      }
				    }
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
			      case ADDR_EXPR:
			        {
				  switch (TREE_CODE (_p1))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _p1))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q40 = gimple_assign_rhs1 (_a3);
						  _q40 = do_valueize (valueize, _q40);
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
								gimple_seq *lseq = seq;
								if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2135;
								{
								  tree tem;
								  tem =  build_int_cst_type (type, diff);
								  res_op->set_value (tem);
								  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1051, __FILE__, __LINE__, true);
								  return true;
								}
next_after_fail2135:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2136;
						{
						  tree tem;
						  tem =  build_int_cst_type (type, diff);
						  res_op->set_value (tem);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1051, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2136:;
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
		    case ADDR_EXPR:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    CASE_CONVERT:
				      {
					tree _q40 = gimple_assign_rhs1 (_a2);
					_q40 = do_valueize (valueize, _q40);
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
						      gimple_seq *lseq = seq;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2137;
						      {
							tree tem;
							tem =  build_int_cst_type (type, diff);
							res_op->set_value (tem);
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1051, __FILE__, __LINE__, true);
							return true;
						      }
next_after_fail2137:;
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
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2138;
				      {
					tree tem;
					tem =  build_int_cst_type (type, diff);
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1051, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2138:;
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
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  switch (TREE_CODE (_q40))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q40))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
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
								gimple_seq *lseq = seq;
								if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2139;
								{
								  tree tem;
								  tem =  build_int_cst_type (type, diff);
								  res_op->set_value (tem);
								  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1052, __FILE__, __LINE__, true);
								  return true;
								}
next_after_fail2139:;
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
						      gimple_seq *lseq = seq;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2140;
						      {
							tree tem;
							tem =  build_int_cst_type (type, diff);
							res_op->set_value (tem);
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1052, __FILE__, __LINE__, true);
							return true;
						      }
next_after_fail2140:;
						    }
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2141;
						{
						  tree tem;
						  tem =  build_int_cst_type (type, diff);
						  res_op->set_value (tem);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1052, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2141:;
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
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2142;
				      {
					tree tem;
					tem =  build_int_cst_type (type, diff);
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1052, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2142:;
				    }
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
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2143;
						{
						  tree tem;
						  tem =  build_int_cst_type (type, diff);
						  res_op->set_value (tem);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1051, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2143:;
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
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2144;
				{
				  tree tem;
				  tem =  build_int_cst_type (type, diff);
				  res_op->set_value (tem);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1051, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2144:;
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
    case ADDR_EXPR:
      {
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
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2145;
				      {
					tree tem;
					tem =  build_int_cst_type (type, diff);
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1051, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2145:;
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
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2146;
		      {
			tree tem;
			tem =  build_int_cst_type (type, diff);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1051, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail2146:;
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
		  switch (TREE_CODE (_q30))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q30))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2147;
						{
						  tree tem;
						  tem =  build_int_cst_type (type, diff);
						  res_op->set_value (tem);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1052, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2147:;
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
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2148;
				      {
					tree tem;
					tem =  build_int_cst_type (type, diff);
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1052, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2148:;
				    }
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
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2149;
				{
				  tree tem;
				  tem =  build_int_cst_type (type, diff);
				  res_op->set_value (tem);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1052, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2149:;
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
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2150;
		      {
			tree tem;
			tem =  build_int_cst_type (type, diff);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1052, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail2150:;
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
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case POINTER_PLUS_EXPR:
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
			      case ADDR_EXPR:
			        {
				  switch (TREE_CODE (_p1))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _p1))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case POINTER_PLUS_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
						  switch (TREE_CODE (_q50))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q50))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
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
									    gimple_seq *lseq = seq;
									    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2151;
									    {
									      res_op->set_op (PLUS_EXPR, type, 2);
									      res_op->ops[0] =  build_int_cst_type (type, diff);
									      {
										tree _o1[1], _r1;
										{
										  tree _o2[2], _r2;
										  _o2[0] = captures[1];
										  _o2[1] = captures[3];
										  gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
										  tem_op.resimplify (lseq, valueize);
										  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
										  if (!_r2) goto next_after_fail2151;
										  _o1[0] = _r2;
										}
										if (type != TREE_TYPE (_o1[0])
										    && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
										  {
										    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
										    tem_op.resimplify (lseq, valueize);
										    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
										    if (!_r1) goto next_after_fail2151;
										  }
										else
										  _r1 = _o1[0];
										res_op->ops[1] = _r1;
									      }
									      res_op->resimplify (lseq, valueize);
									      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1053, __FILE__, __LINE__, true);
									      return true;
									    }
next_after_fail2151:;
									  }
								    }
								  }
							          break;
							        }
							      default:;
							      }
						        }
						      break;
						    case ADDR_EXPR:
						      {
							{
							  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51 };
							  {
 poly_int64 diff;
							      if (ptr_difference_const (captures[0], captures[2], &diff)
)
								{
								  gimple_seq *lseq = seq;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2152;
								  {
								    res_op->set_op (PLUS_EXPR, type, 2);
								    res_op->ops[0] =  build_int_cst_type (type, diff);
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] = captures[1];
									_o2[1] = captures[3];
									gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
									tem_op.resimplify (lseq, valueize);
									_r2 = maybe_push_res_to_seq (&tem_op, lseq);
									if (!_r2) goto next_after_fail2152;
									_o1[0] = _r2;
								      }
								      if (type != TREE_TYPE (_o1[0])
								          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
									{
									  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
									  tem_op.resimplify (lseq, valueize);
									  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
									  if (!_r1) goto next_after_fail2152;
								        }
								      else
								        _r1 = _o1[0];
								      res_op->ops[1] = _r1;
								    }
								    res_op->resimplify (lseq, valueize);
								    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1053, __FILE__, __LINE__, true);
								    return true;
								  }
next_after_fail2152:;
								}
							  }
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
		    case ADDR_EXPR:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    case POINTER_PLUS_EXPR:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_assign_rhs2 (_a2);
					_q51 = do_valueize (valueize, _q51);
					switch (TREE_CODE (_q50))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _q50))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
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
								  gimple_seq *lseq = seq;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2153;
								  {
								    res_op->set_op (PLUS_EXPR, type, 2);
								    res_op->ops[0] =  build_int_cst_type (type, diff);
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] = captures[1];
									_o2[1] = captures[3];
									gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
									tem_op.resimplify (lseq, valueize);
									_r2 = maybe_push_res_to_seq (&tem_op, lseq);
									if (!_r2) goto next_after_fail2153;
									_o1[0] = _r2;
								      }
								      if (type != TREE_TYPE (_o1[0])
								          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
									{
									  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
									  tem_op.resimplify (lseq, valueize);
									  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
									  if (!_r1) goto next_after_fail2153;
								        }
								      else
								        _r1 = _o1[0];
								      res_op->ops[1] = _r1;
								    }
								    res_op->resimplify (lseq, valueize);
								    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1053, __FILE__, __LINE__, true);
								    return true;
								  }
next_after_fail2153:;
								}
							  }
							}
						        break;
						      }
						    default:;
						    }
					      }
					    break;
					  case ADDR_EXPR:
					    {
					      {
						tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51 };
						{
 poly_int64 diff;
						    if (ptr_difference_const (captures[0], captures[2], &diff)
)
						      {
							gimple_seq *lseq = seq;
							if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2154;
							{
							  res_op->set_op (PLUS_EXPR, type, 2);
							  res_op->ops[0] =  build_int_cst_type (type, diff);
							  {
							    tree _o1[1], _r1;
							    {
							      tree _o2[2], _r2;
							      _o2[0] = captures[1];
							      _o2[1] = captures[3];
							      gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
							      tem_op.resimplify (lseq, valueize);
							      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
							      if (!_r2) goto next_after_fail2154;
							      _o1[0] = _r2;
							    }
							    if (type != TREE_TYPE (_o1[0])
							        && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
							      {
								gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
								tem_op.resimplify (lseq, valueize);
								_r1 = maybe_push_res_to_seq (&tem_op, lseq);
								if (!_r1) goto next_after_fail2154;
							      }
							    else
							      _r1 = _o1[0];
							    res_op->ops[1] = _r1;
							  }
							  res_op->resimplify (lseq, valueize);
							  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1053, __FILE__, __LINE__, true);
							  return true;
							}
next_after_fail2154:;
						      }
						}
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
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
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
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2155;
					    {
					      res_op->set_op (PLUS_EXPR, type, 2);
					      res_op->ops[0] =  build_int_cst_type (type, diff);
					      {
						tree _o1[1], _r1;
						_o1[0] = captures[1];
						if (type != TREE_TYPE (_o1[0])
						    && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
						  {
						    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2155;
						  }
						else
						  _r1 = _o1[0];
						res_op->ops[1] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1054, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2155:;
					  }
				    }
				  }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    case ADDR_EXPR:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			  {
 poly_int64 diff;
			      if (ptr_difference_const (captures[0], captures[2], &diff)
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2156;
				  {
				    res_op->set_op (PLUS_EXPR, type, 2);
				    res_op->ops[0] =  build_int_cst_type (type, diff);
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[1];
				      if (type != TREE_TYPE (_o1[0])
				          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					{
					  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail2156;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1054, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail2156:;
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
				    gimple_seq *lseq = seq;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2157;
				    {
				      res_op->set_op (NOP_EXPR, type, 1);
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[1];
					gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, stype, _o1[0]);
					tem_op.resimplify (lseq, valueize);
					_r1 = maybe_push_res_to_seq (&tem_op, lseq);
					if (!_r1) goto next_after_fail2157;
					res_op->ops[0] = _r1;
				      }
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1055, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail2157:;
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
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
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
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
				    {
 poly_int64 diff;
					if (ptr_difference_const (captures[0], captures[1], &diff)
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2158;
					    {
					      res_op->set_op (MINUS_EXPR, type, 2);
					      res_op->ops[0] =  build_int_cst_type (type, diff);
					      {
						tree _o1[1], _r1;
						_o1[0] = captures[2];
						if (type != TREE_TYPE (_o1[0])
						    && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
						  {
						    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2158;
						  }
						else
						  _r1 = _o1[0];
						res_op->ops[1] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1056, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2158:;
					  }
				    }
				  }
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
    case ADDR_EXPR:
      {
	switch (TREE_CODE (_p1))
	  {
	  case SSA_NAME:
	    if (gimple *_d1 = get_def (valueize, _p1))
	      {
		if (gassign *_a1 = dyn_cast <gassign *> (_d1))
		  switch (gimple_assign_rhs_code (_a1))
		    {
		    case POINTER_PLUS_EXPR:
		      {
			tree _q30 = gimple_assign_rhs1 (_a1);
			_q30 = do_valueize (valueize, _q30);
			tree _q31 = gimple_assign_rhs2 (_a1);
			_q31 = do_valueize (valueize, _q31);
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
			  {
 poly_int64 diff;
			      if (ptr_difference_const (captures[0], captures[1], &diff)
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2159;
				  {
				    res_op->set_op (MINUS_EXPR, type, 2);
				    res_op->ops[0] =  build_int_cst_type (type, diff);
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[2];
				      if (type != TREE_TYPE (_o1[0])
				          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					{
					  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail2159;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1056, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail2159:;
				}
			  }
			}
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case POINTER_PLUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
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
				    gimple_seq *lseq = seq;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2160;
				    {
				      res_op->set_op (NEGATE_EXPR, type, 1);
				      {
					tree _o1[1], _r1;
					{
					  tree _o2[1], _r2;
					  _o2[0] = captures[1];
					  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, stype, _o2[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r2) goto next_after_fail2160;
					  _o1[0] = _r2;
					}
					if (type != TREE_TYPE (_o1[0])
					    && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					  {
					    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2160;
					  }
					else
					  _r1 = _o1[0];
					res_op->ops[0] = _r1;
				      }
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1057, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail2160:;
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case POINTER_PLUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case POINTER_PLUS_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2161;
						{
						  res_op->set_op (POINTER_DIFF_EXPR, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[2];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1058, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2161:;
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
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2162;
						    {
						      res_op->set_op (MINUS_EXPR, type, 2);
						      {
							tree _o1[1], _r1;
							{
							  tree _o2[1], _r2;
							  _o2[0] = captures[1];
							  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, stype, _o2[0]);
							  tem_op.resimplify (lseq, valueize);
							  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
							  if (!_r2) goto next_after_fail2162;
							  _o1[0] = _r2;
							}
							if (type != TREE_TYPE (_o1[0])
							    && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
							  {
							    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
							    tem_op.resimplify (lseq, valueize);
							    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
							    if (!_r1) goto next_after_fail2162;
							  }
							else
							  _r1 = _o1[0];
							res_op->ops[0] = _r1;
						      }
						      {
							tree _o1[1], _r1;
							{
							  tree _o2[1], _r2;
							  _o2[0] = captures[2];
							  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, stype, _o2[0]);
							  tem_op.resimplify (lseq, valueize);
							  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
							  if (!_r2) goto next_after_fail2162;
							  _o1[0] = _r2;
							}
							if (type != TREE_TYPE (_o1[0])
							    && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
							  {
							    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
							    tem_op.resimplify (lseq, valueize);
							    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
							    if (!_r1) goto next_after_fail2162;
							  }
							else
							  _r1 = _o1[0];
							res_op->ops[1] = _r1;
						      }
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1059, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2162:;
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
if (integer_zerop (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p1 };
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2163;
      {
	res_op->set_op (NEGATE_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  if (type != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail2163;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1060, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail2163:;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGNF (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2355;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2355:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2356;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2356:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGNF:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGN (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGN;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2357;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2357:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGN;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2358;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2358:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN))
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
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGNL (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNL;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2359;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2359:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNL;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2360;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2360:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGNL:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGNF16 (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF16;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2361;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2361:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF16;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2362;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2362:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF16))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF16))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGNF16:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF16))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF16))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF16))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF16))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGNF32 (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF32;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2363;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2363:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF32;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2364;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2364:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGNF32:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGNF64 (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF64;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2365;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2365:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF64;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2366;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2366:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGNF64:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGNF128 (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF128;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2367;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2367:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF128;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2368;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2368:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGNF128:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGNF32X (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF32X;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2369;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2369:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF32X;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2370;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2370:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32X))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32X))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGNF32X:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32X))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32X))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32X))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGNF64X (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF64X;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2371;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2371:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF64X;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2372;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2372:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64X))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64X))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGNF64X:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64X))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64X))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64X))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COPYSIGNF128X (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF128X;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2373;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2373:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF128X;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2374;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2374:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128X))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128X))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGNF128X:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128X))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128X))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128X))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_COPYSIGN (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn copysigns = CFN_COPYSIGN;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2375;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2375:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn copysigns = CFN_COPYSIGN;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2376;
		    {
		      res_op->set_op (copysigns, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1090, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2376:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_307 (res_op, seq, valueize, type, captures, CFN_COPYSIGN))
	    return true;
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
	  if (gimple_simplify_308 (res_op, seq, valueize, type, captures, CFN_COPYSIGN))
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
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_309 (res_op, seq, valueize, type, captures, CFN_COPYSIGN))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_310 (res_op, seq, valueize, type, captures, CFN_COPYSIGN))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			if (gimple_simplify_311 (res_op, seq, valueize, type, captures, CFN_COPYSIGN))
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
if (tree_expr_nonnegative_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_312 (res_op, seq, valueize, type, captures, CFN_COPYSIGN))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_TRUNC_DIV_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_193 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  switch (TREE_CODE (_q40))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q40))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case LSHIFT_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
						  if (integer_onep (_q50))
						    {
						      {
							tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q50, _q51 };
							if (gimple_simplify_313 (res_op, seq, valueize, type, captures))
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
			          break;
			        }
			      case LSHIFT_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (integer_onep (_q40))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _q41 };
					if (gimple_simplify_313 (res_op, seq, valueize, type, captures))
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
		  switch (TREE_CODE (_q30))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q30))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case LSHIFT_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (integer_onep (_q40))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q40, _q41 };
					if (gimple_simplify_314 (res_op, seq, valueize, type, captures))
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
	          break;
	        }
	      case LSHIFT_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (integer_onep (_q30))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q30, _q31 };
			if (gimple_simplify_314 (res_op, seq, valueize, type, captures))
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
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_315 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_317 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
if (integer_minus_onep (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_316 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
        return true;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_318 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
	      case ABS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case TRUNC_DIV_EXPR:
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_321 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
						if (gimple_simplify_322 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_322 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
	      case MULT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (integer_pow2p (_q21))
		    {
		      if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
			      {
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2377;
				{
				  res_op->set_op (BIT_AND_EXPR, type, 2);
				  res_op->ops[0] = captures[0];
				  res_op->ops[1] =  wide_int_to_tree
 (type, wi::mask (TYPE_PRECISION (type)
 - wi::exact_log2 (wi::to_wide (captures[1])),
 false, TYPE_PRECISION (type)));
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1091, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2377:;
			      }
			  }
		        }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
			  return true;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case INTEGER_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    case MULT_EXPR:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_assign_rhs2 (_a2);
					_q51 = do_valueize (valueize, _q51);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
					  {
					    switch (TREE_CODE (_q51))
					      {
					      case INTEGER_CST:
					        {
						  {
						    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
						    if (gimple_simplify_325 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
						      return true;
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
			    break;
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_324 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
	      case PLUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  switch (TREE_CODE (_q21))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case MULT_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41 };
					if (gimple_simplify_326 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
					  return true;
				      }
				    }
				  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q41, _q40 };
					if (gimple_simplify_326 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case MULT_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (tree_swap_operands_p (_q30, _q31))
				    std::swap (_q30, _q31);
				  if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q30, _q31 };
					if (gimple_simplify_326 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
					  return true;
				      }
				    }
				  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q31, _q30 };
					if (gimple_simplify_326 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
				{
 wide_int c = wi::to_wide (captures[2]);
 wide_int n = wi::to_wide (captures[3]);
 bool shift = TRUNC_DIV_EXPR == RSHIFT_EXPR;
 int_range_max vr0, vr1, vr3;
				    if (INTEGRAL_TYPE_P (type)
 && get_range_query (cfun)->range_of_expr (vr0, captures[1])
)
				      {
					if ((shift ? wi::ctz (
c
) >= n.to_shwi ()
 : wi::multiple_of_p (
c
, n, TYPE_SIGN (type)))
 && get_range_query (cfun)->range_of_expr (vr1, captures[2])
 && range_op_handler (PLUS_EXPR).overflow_free_p (vr0, vr1)
 && get_range_query (cfun)->range_of_expr (vr3, captures[0])
 && !vr3.undefined_p ()
 && (TYPE_UNSIGNED (type)
 || (vr0.nonnegative_p () && vr3.nonnegative_p ())
 || (vr0.nonpositive_p () && vr3.nonpositive_p ()))
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2378;
					    {
					      res_op->set_op (PLUS_EXPR, type, 2);
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[1];
						_o1[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (lseq, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r1) goto next_after_fail2378;
						res_op->ops[0] = _r1;
					      }
					      res_op->ops[1] =  wide_int_to_tree (type,
(shift ? wi::rshift (
c
, n, TYPE_SIGN (type))
 : wi::div_trunc (
c
, n, TYPE_SIGN (type)))
);
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1092, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2378:;
					  }
					else
					  {
					    if (!vr0.undefined_p () && TYPE_UNSIGNED (type) && c.sign_mask () < 0
 &&
(shift ? wi::ctz (
-c
) >= n.to_shwi ()
 : wi::multiple_of_p (
-c
, n, TYPE_SIGN (type)))
 && wi::geu_p (vr0.lower_bound (), -c)
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2379;
						{
						  res_op->set_op (PLUS_EXPR, type, 2);
						  {
						    tree _o1[2], _r1;
						    _o1[0] = captures[1];
						    _o1[1] = captures[3];
						    gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2379;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  wide_int_to_tree (type, -
(shift ? wi::rshift (
-c
, n, TYPE_SIGN (type))
 : wi::div_trunc (
-c
, n, TYPE_SIGN (type)))
);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1093, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2379:;
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
	      case MINUS_EXPR:
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
			      case MULT_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41 };
					if (gimple_simplify_327 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
					  return true;
				      }
				    }
				  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q41, _q40 };
					if (gimple_simplify_327 (res_op, seq, valueize, type, captures, TRUNC_DIV_EXPR))
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
 && (TREE_CODE_CLASS (TRUNC_DIV_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2380;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2380;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2380;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2380:;
					  }
				      }
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
 && (TREE_CODE_CLASS (TRUNC_DIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2381;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2381;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2381;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2381:;
		      }
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2382;
	      {
		res_op->set_op (NOP_EXPR, type, 1);
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[1];
		  _o1[1] =  build_one_cst (type);
		  gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, boolean_type_node, _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail2382;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1094, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2382:;
	    }
	  else
	    {
	      if (!canonicalize_math_p ()
)
		{
		  {
 tree utype = unsigned_type_for (type);
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2383;
		      {
			res_op->set_op (COND_EXPR, type, 3);
			{
			  tree _o1[2], _r1;
			  {
			    tree _o2[2], _r2;
			    {
			      tree _o3[1], _r3;
			      _o3[0] = captures[1];
			      if (utype != TREE_TYPE (_o3[0])
			          && !useless_type_conversion_p (utype, TREE_TYPE (_o3[0])))
				{
				  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o3[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r3) goto next_after_fail2383;
			        }
			      else
			        _r3 = _o3[0];
			      _o2[0] = _r3;
			    }
			    _o2[1] =  build_one_cst (utype);
			    gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail2383;
			    _o1[0] = _r2;
			  }
			  _o1[1] =  build_int_cst (utype, 2);
			  gimple_match_op tem_op (res_op->cond.any_else (), LE_EXPR, boolean_type_node, _o1[0], _o1[1]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail2383;
			  res_op->ops[0] = _r1;
			}
			res_op->ops[1] = captures[1];
			res_op->ops[2] =  build_zero_cst (type);
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1095, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail2383:;
		  }
		}
	    }
	}
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
 && (TREE_CODE_CLASS (TRUNC_DIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2384;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2384;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2384;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2384:;
		      }
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
gimple_simplify_CEIL_DIV_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_193 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
	  return true;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_315 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_317 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
if (integer_minus_onep (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_316 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
        return true;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_318 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
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
	      case ABS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
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
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
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
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
			  return true;
		      }
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
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
						if (gimple_simplify_322 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
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
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_322 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
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
	      case MULT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
			  return true;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case INTEGER_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    case MULT_EXPR:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_assign_rhs2 (_a2);
					_q51 = do_valueize (valueize, _q51);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
					  {
					    switch (TREE_CODE (_q51))
					      {
					      case INTEGER_CST:
					        {
						  {
						    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
						    if (gimple_simplify_325 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
						      return true;
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
			    break;
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_324 (res_op, seq, valueize, type, captures, CEIL_DIV_EXPR))
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
 && (TREE_CODE_CLASS (CEIL_DIV_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2385;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2385;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2385;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2385:;
					  }
				      }
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
 && (TREE_CODE_CLASS (CEIL_DIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2386;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2386;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2386;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2386:;
		      }
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
 && (TREE_CODE_CLASS (CEIL_DIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2387;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2387;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2387;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2387:;
		      }
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
gimple_simplify_FLOOR_DIV_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_193 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
	  return true;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_315 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_317 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
if (integer_minus_onep (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_316 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
        return true;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_318 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
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
	      case ABS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
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
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
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
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
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
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_328 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR, TRUNC_DIV_EXPR))
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
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
						if (gimple_simplify_322 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
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
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_322 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
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
	      case MULT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
			  return true;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case INTEGER_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    case MULT_EXPR:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_assign_rhs2 (_a2);
					_q51 = do_valueize (valueize, _q51);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
					  {
					    switch (TREE_CODE (_q51))
					      {
					      case INTEGER_CST:
					        {
						  {
						    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
						    if (gimple_simplify_325 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
						      return true;
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
			    break;
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_324 (res_op, seq, valueize, type, captures, FLOOR_DIV_EXPR))
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
 && (TREE_CODE_CLASS (FLOOR_DIV_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2388;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2388;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2388;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2388:;
					  }
				      }
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
 && (TREE_CODE_CLASS (FLOOR_DIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2389;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2389;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2389;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2389:;
		      }
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
 && (TREE_CODE_CLASS (FLOOR_DIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2390;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2390;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2390;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2390:;
		      }
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
gimple_simplify_ROUND_DIV_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_193 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
	  return true;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_315 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_317 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
if (integer_minus_onep (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_316 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
        return true;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_318 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
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
	      case ABS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
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
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
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
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
			  return true;
		      }
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
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
						if (gimple_simplify_322 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
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
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_322 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
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
	      case MULT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
			  return true;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case INTEGER_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    case MULT_EXPR:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_assign_rhs2 (_a2);
					_q51 = do_valueize (valueize, _q51);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
					  {
					    switch (TREE_CODE (_q51))
					      {
					      case INTEGER_CST:
					        {
						  {
						    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
						    if (gimple_simplify_325 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
						      return true;
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
			    break;
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_324 (res_op, seq, valueize, type, captures, ROUND_DIV_EXPR))
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
 && (TREE_CODE_CLASS (ROUND_DIV_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2391;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2391;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2391;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2391:;
					  }
				      }
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
 && (TREE_CODE_CLASS (ROUND_DIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2392;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2392;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2392;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2392:;
		      }
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
 && (TREE_CODE_CLASS (ROUND_DIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2393;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2393;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2393;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2393:;
		      }
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
gimple_simplify_EXACT_DIV_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_193 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
	  return true;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_315 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_317 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
if (integer_minus_onep (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_316 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
        return true;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_318 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
	      case ABS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_319 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_320 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case EXACT_DIV_EXPR:
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_321 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
						if (gimple_simplify_322 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_322 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
	      case MULT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_323 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
			  return true;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case INTEGER_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    case MULT_EXPR:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_assign_rhs2 (_a2);
					_q51 = do_valueize (valueize, _q51);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
					  {
					    switch (TREE_CODE (_q51))
					      {
					      case INTEGER_CST:
					        {
						  {
						    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
						    if (gimple_simplify_325 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
						      return true;
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
			    break;
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_324 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
	      case PLUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  switch (TREE_CODE (_q21))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case MULT_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41 };
					if (gimple_simplify_326 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
					  return true;
				      }
				    }
				  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q41, _q40 };
					if (gimple_simplify_326 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case MULT_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (tree_swap_operands_p (_q30, _q31))
				    std::swap (_q30, _q31);
				  if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q30, _q31 };
					if (gimple_simplify_326 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
					  return true;
				      }
				    }
				  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q31, _q30 };
					if (gimple_simplify_326 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
				{
 wide_int c = wi::to_wide (captures[2]);
 wide_int n = wi::to_wide (captures[3]);
 bool shift = EXACT_DIV_EXPR == RSHIFT_EXPR;
 int_range_max vr0, vr1, vr3;
				    if (INTEGRAL_TYPE_P (type)
 && get_range_query (cfun)->range_of_expr (vr0, captures[1])
)
				      {
					if ((shift ? wi::ctz (
c
) >= n.to_shwi ()
 : wi::multiple_of_p (
c
, n, TYPE_SIGN (type)))
 && get_range_query (cfun)->range_of_expr (vr1, captures[2])
 && range_op_handler (PLUS_EXPR).overflow_free_p (vr0, vr1)
 && get_range_query (cfun)->range_of_expr (vr3, captures[0])
 && !vr3.undefined_p ()
 && (TYPE_UNSIGNED (type)
 || (vr0.nonnegative_p () && vr3.nonnegative_p ())
 || (vr0.nonpositive_p () && vr3.nonpositive_p ()))
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2394;
					    {
					      res_op->set_op (PLUS_EXPR, type, 2);
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[1];
						_o1[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), EXACT_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (lseq, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r1) goto next_after_fail2394;
						res_op->ops[0] = _r1;
					      }
					      res_op->ops[1] =  wide_int_to_tree (type,
(shift ? wi::rshift (
c
, n, TYPE_SIGN (type))
 : wi::div_trunc (
c
, n, TYPE_SIGN (type)))
);
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1092, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2394:;
					  }
					else
					  {
					    if (!vr0.undefined_p () && TYPE_UNSIGNED (type) && c.sign_mask () < 0
 &&
(shift ? wi::ctz (
-c
) >= n.to_shwi ()
 : wi::multiple_of_p (
-c
, n, TYPE_SIGN (type)))
 && wi::geu_p (vr0.lower_bound (), -c)
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2395;
						{
						  res_op->set_op (PLUS_EXPR, type, 2);
						  {
						    tree _o1[2], _r1;
						    _o1[0] = captures[1];
						    _o1[1] = captures[3];
						    gimple_match_op tem_op (res_op->cond.any_else (), EXACT_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2395;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  wide_int_to_tree (type, -
(shift ? wi::rshift (
-c
, n, TYPE_SIGN (type))
 : wi::div_trunc (
-c
, n, TYPE_SIGN (type)))
);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1093, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2395:;
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
	      case MINUS_EXPR:
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
			      case MULT_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41 };
					if (gimple_simplify_327 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
					  return true;
				      }
				    }
				  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q41, _q40 };
					if (gimple_simplify_327 (res_op, seq, valueize, type, captures, EXACT_DIV_EXPR))
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
gimple_simplify_FLOOR_MOD_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_328 (res_op, seq, valueize, type, captures, FLOOR_MOD_EXPR, TRUNC_MOD_EXPR))
      return true;
  }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_329 (res_op, seq, valueize, type, captures, FLOOR_MOD_EXPR))
	  return true;
      }
    }
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_330 (res_op, seq, valueize, type, captures, FLOOR_MOD_EXPR))
	  return true;
      }
    }
  if (integer_minus_onep (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_331 (res_op, seq, valueize, type, captures, FLOOR_MOD_EXPR))
	  return true;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_332 (res_op, seq, valueize, type, captures, FLOOR_MOD_EXPR))
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
	      case FLOOR_MOD_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_333 (res_op, seq, valueize, type, captures, FLOOR_MOD_EXPR))
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
				if (gimple_simplify_334 (res_op, seq, valueize, type, captures, FLOOR_MOD_EXPR))
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
		    tree _q30_pops[1];
		    if (gimple_power_of_two_cand (_q30, _q30_pops, valueize))
		      {
			tree _q40 = _q30_pops[0];
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q40 };
			  if (gimple_simplify_335 (res_op, seq, valueize, type, captures, FLOOR_MOD_EXPR))
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
{
  tree _p1_pops[1];
  if (gimple_power_of_two_cand (_p1, _p1_pops, valueize))
    {
      tree _q30 = _p1_pops[0];
      {
	tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
	if (gimple_simplify_335 (res_op, seq, valueize, type, captures, FLOOR_MOD_EXPR))
	  return true;
      }
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
 && (TREE_CODE_CLASS (FLOOR_MOD_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2396;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2396;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2396;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2396:;
					  }
				      }
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
 && (TREE_CODE_CLASS (FLOOR_MOD_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2397;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2397;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2397;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2397:;
		      }
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
 && (TREE_CODE_CLASS (FLOOR_MOD_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2398;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2398;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2398;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2398:;
		      }
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
gimple_simplify_RDIV_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
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
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2399;
	    {
	      tree tem;
	      tem =  build_one_cst (type);
	      res_op->set_value (tem);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1096, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2399:;
	  }
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_336 (res_op, seq, valueize, type, captures))
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
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_336 (res_op, seq, valueize, type, captures))
			  return true;
		      }
		    }
	          break;
	        }
	      CASE_CONVERT:
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
			      CASE_CONVERT:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  switch (TREE_CODE (_q40))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q40))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case ABS_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
						    {
						      {
							tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
							if (gimple_simplify_337 (res_op, seq, valueize, type, captures))
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
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
		    }
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case ABS_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  switch (TREE_CODE (_p1))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _p1))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  if ((_q50 == _q30 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q30, 0) && types_match (_q50, _q30)))
						    {
						      {
							tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
							if (gimple_simplify_337 (res_op, seq, valueize, type, captures))
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case ABS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_337 (res_op, seq, valueize, type, captures))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_337 (res_op, seq, valueize, type, captures))
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
  if (real_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (!tree_expr_maybe_signaling_nan_p (captures[0])
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2400;
	    {
	      tree tem;
	      tem = captures[0];
	      res_op->set_value (tem);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1097, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2400:;
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
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2401;
	    {
	      res_op->set_op (NEGATE_EXPR, type, 1);
	      res_op->ops[0] = captures[0];
	      res_op->resimplify (lseq, valueize);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1098, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2401:;
	  }
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
	      case RDIV_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		    if (flag_reciprocal_math
)
		      {
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[0])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2402;
			{
			  res_op->set_op (RDIV_EXPR, type, 2);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (lseq, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r1) goto next_after_fail2402;
			    res_op->ops[1] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1099, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2402:;
		      }
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
	      case MULT_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
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
				      gimple_seq *lseq = seq;
				      if (lseq
				          && (!single_use (captures[1])))
				        lseq = NULL;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2403;
				      {
					res_op->set_op (RDIV_EXPR, type, 2);
					{
					  tree _o1[2], _r1;
					  _o1[0] = captures[0];
					  _o1[1] =  tem;
					  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail2403;
					  res_op->ops[0] = _r1;
					}
					res_op->ops[1] = captures[2];
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1100, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2403:;
				    }
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
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		    if (flag_reciprocal_math
)
		      {
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[1])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2404;
			{
			  res_op->set_op (MULT_EXPR, type, 2);
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[2];
			    gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (lseq, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r1) goto next_after_fail2404;
			    res_op->ops[0] = _r1;
			  }
			  res_op->ops[1] = captures[3];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1101, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2404:;
		      }
		  }
	          break;
	        }
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2405;
		    {
		      res_op->set_op (RDIV_EXPR, type, 2);
		      {
			tree _o1[1], _r1;
			_o1[0] = captures[0];
			gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
			tem_op.resimplify (lseq, valueize);
			_r1 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r1) goto next_after_fail2405;
			res_op->ops[0] = _r1;
		      }
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1102, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2405:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
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
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2406;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] =  tem;
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1103, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2406:;
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
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2407;
			      {
				res_op->set_op (MULT_EXPR, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] =  inverse;
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1104, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2407:;
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
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2408;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] =  tem;
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1103, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2408:;
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
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2409;
			      {
				res_op->set_op (MULT_EXPR, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] =  inverse;
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1104, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2409:;
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
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2410;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] =  tem;
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1103, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2410:;
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
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2411;
			      {
				res_op->set_op (MULT_EXPR, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] =  inverse;
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1104, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2411:;
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
 && (TREE_CODE_CLASS (RDIV_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2412;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2412;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2412;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2412:;
					  }
				      }
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
 && (TREE_CODE_CLASS (RDIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2413;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2413;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2413;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2413:;
		      }
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
 && (TREE_CODE_CLASS (RDIV_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2414;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2414;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2414;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2414:;
		      }
		  }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_SQRTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			  if (gimple_simplify_338 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF))
			    return true;
			}
		      }
		    switch (TREE_CODE (_q30))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _q30))
		          {
			    if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			      switch (gimple_assign_rhs_code (_a2))
				{
				case RDIV_EXPR:
				  {
				    tree _q40 = gimple_assign_rhs1 (_a2);
				    _q40 = do_valueize (valueize, _q40);
				    tree _q41 = gimple_assign_rhs2 (_a2);
				    _q41 = do_valueize (valueize, _q41);
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
				      if (flag_unsafe_math_optimizations
)
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[1])
					          || !single_use (captures[2])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2415;
					  {
					    res_op->set_op (MULT_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						_o2[0] = captures[4];
						_o2[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail2415;
						_o1[0] = _r2;
					      }
					      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_SQRTF, TREE_TYPE (_o1[0]), _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2415;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1105, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail2415:;
					}
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
	      case CFN_BUILT_IN_SQRTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			  if (gimple_simplify_338 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL))
			    return true;
			}
		      }
		    switch (TREE_CODE (_q30))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _q30))
		          {
			    if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			      switch (gimple_assign_rhs_code (_a2))
				{
				case RDIV_EXPR:
				  {
				    tree _q40 = gimple_assign_rhs1 (_a2);
				    _q40 = do_valueize (valueize, _q40);
				    tree _q41 = gimple_assign_rhs2 (_a2);
				    _q41 = do_valueize (valueize, _q41);
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
				      if (flag_unsafe_math_optimizations
)
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[1])
					          || !single_use (captures[2])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2416;
					  {
					    res_op->set_op (MULT_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						_o2[0] = captures[4];
						_o2[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail2416;
						_o1[0] = _r2;
					      }
					      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_SQRTL, TREE_TYPE (_o1[0]), _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2416;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1105, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail2416:;
					}
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
	      case CFN_BUILT_IN_EXP:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2417;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2417;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXP, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2417;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2417:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP10F:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2418;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2418;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXP10F, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2418;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2418:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP10L:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2419;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2419;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXP10L, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2419;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2419:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CBRT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    switch (TREE_CODE (_q30))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _q30))
		          {
			    if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			      switch (gimple_assign_rhs_code (_a2))
				{
				case RDIV_EXPR:
				  {
				    tree _q40 = gimple_assign_rhs1 (_a2);
				    _q40 = do_valueize (valueize, _q40);
				    tree _q41 = gimple_assign_rhs2 (_a2);
				    _q41 = do_valueize (valueize, _q41);
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
				      if (flag_unsafe_math_optimizations
)
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[1])
					          || !single_use (captures[2])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2420;
					  {
					    res_op->set_op (MULT_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						_o2[0] = captures[4];
						_o2[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail2420;
						_o1[0] = _r2;
					      }
					      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_CBRT, TREE_TYPE (_o1[0]), _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2420;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1105, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail2420:;
					}
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
	      case CFN_BUILT_IN_EXP2:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2421;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2421;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXP2, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2421;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2421:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXPF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2422;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2422;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXPF, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2422;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2422:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXPL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2423;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2423;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXPL, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2423;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2423:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_SQRT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			  if (gimple_simplify_338 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT))
			    return true;
			}
		      }
		    switch (TREE_CODE (_q30))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _q30))
		          {
			    if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			      switch (gimple_assign_rhs_code (_a2))
				{
				case RDIV_EXPR:
				  {
				    tree _q40 = gimple_assign_rhs1 (_a2);
				    _q40 = do_valueize (valueize, _q40);
				    tree _q41 = gimple_assign_rhs2 (_a2);
				    _q41 = do_valueize (valueize, _q41);
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
				      if (flag_unsafe_math_optimizations
)
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[1])
					          || !single_use (captures[2])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2424;
					  {
					    res_op->set_op (MULT_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						_o2[0] = captures[4];
						_o2[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail2424;
						_o1[0] = _r2;
					      }
					      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_SQRT, TREE_TYPE (_o1[0]), _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2424;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1105, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail2424:;
					}
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
	      case CFN_EXP:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2425;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2425;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_EXP, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2425;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2425:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POW10F:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2426;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2426;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_POW10F, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2426;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2426:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POW10L:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2427;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2427;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_POW10L, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2427;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2427:;
			}
		    }
	          }
	        break;
	      case CFN_EXP2:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2428;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2428;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_EXP2, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2428;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2428:;
			}
		    }
	          }
	        break;
	      case CFN_SQRT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			  if (gimple_simplify_338 (res_op, seq, valueize, type, captures, CFN_SQRT))
			    return true;
			}
		      }
		    switch (TREE_CODE (_q30))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _q30))
		          {
			    if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			      switch (gimple_assign_rhs_code (_a2))
				{
				case RDIV_EXPR:
				  {
				    tree _q40 = gimple_assign_rhs1 (_a2);
				    _q40 = do_valueize (valueize, _q40);
				    tree _q41 = gimple_assign_rhs2 (_a2);
				    _q41 = do_valueize (valueize, _q41);
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
				      if (flag_unsafe_math_optimizations
)
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[1])
					          || !single_use (captures[2])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2429;
					  {
					    res_op->set_op (MULT_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						_o2[0] = captures[4];
						_o2[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail2429;
						_o1[0] = _r2;
					      }
					      gimple_match_op tem_op (res_op->cond.any_else (), CFN_SQRT, TREE_TYPE (_o1[0]), _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2429;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1105, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail2429:;
					}
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
	      case CFN_EXP10:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2430;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2430;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_EXP10, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2430;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2430:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CBRTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    switch (TREE_CODE (_q30))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _q30))
		          {
			    if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			      switch (gimple_assign_rhs_code (_a2))
				{
				case RDIV_EXPR:
				  {
				    tree _q40 = gimple_assign_rhs1 (_a2);
				    _q40 = do_valueize (valueize, _q40);
				    tree _q41 = gimple_assign_rhs2 (_a2);
				    _q41 = do_valueize (valueize, _q41);
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
				      if (flag_unsafe_math_optimizations
)
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[1])
					          || !single_use (captures[2])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2431;
					  {
					    res_op->set_op (MULT_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						_o2[0] = captures[4];
						_o2[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail2431;
						_o1[0] = _r2;
					      }
					      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_CBRTF, TREE_TYPE (_o1[0]), _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2431;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1105, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail2431:;
					}
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
	      case CFN_BUILT_IN_CBRTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    switch (TREE_CODE (_q30))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _q30))
		          {
			    if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			      switch (gimple_assign_rhs_code (_a2))
				{
				case RDIV_EXPR:
				  {
				    tree _q40 = gimple_assign_rhs1 (_a2);
				    _q40 = do_valueize (valueize, _q40);
				    tree _q41 = gimple_assign_rhs2 (_a2);
				    _q41 = do_valueize (valueize, _q41);
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
				      if (flag_unsafe_math_optimizations
)
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[1])
					          || !single_use (captures[2])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2432;
					  {
					    res_op->set_op (MULT_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						_o2[0] = captures[4];
						_o2[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail2432;
						_o1[0] = _r2;
					      }
					      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_CBRTL, TREE_TYPE (_o1[0]), _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2432;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1105, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail2432:;
					}
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
	      case CFN_BUILT_IN_EXP10:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2433;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2433;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXP10, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2433;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2433:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP2F:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2434;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2434;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXP2F, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2434;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2434:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP2L:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2435;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2435;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXP2L, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2435;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2435:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POW10:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[1])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2436;
			  {
			    res_op->set_op (MULT_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail2436;
				_o1[0] = _r2;
			      }
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_POW10, TREE_TYPE (_o1[0]), _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2436;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1106, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2436:;
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_SINHF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_COSHF:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_341 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SINHF, CFN_BUILT_IN_COSHF, CFN_BUILT_IN_TANHF))
					      return true;
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
	      case CFN_BUILT_IN_SINHL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_COSHL:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_341 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SINHL, CFN_BUILT_IN_COSHL, CFN_BUILT_IN_TANHL))
					      return true;
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
	      case CFN_BUILT_IN_TANHF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SINHF:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_342 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TANHF, CFN_BUILT_IN_SINHF, CFN_BUILT_IN_COSHF))
					      return true;
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
	      case CFN_BUILT_IN_TANHL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SINHL:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_342 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TANHL, CFN_BUILT_IN_SINHL, CFN_BUILT_IN_COSHL))
					      return true;
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
	      case CFN_BUILT_IN_COS:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SIN:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_343 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COS, CFN_BUILT_IN_SIN, CFN_BUILT_IN_TAN))
					      return true;
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
	      case CFN_BUILT_IN_POW:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    switch (TREE_CODE (_q21))
		      {
		      case REAL_CST:
		        {
			  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
				if (gimple_simplify_345 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POW))
				  return true;
			      }
			    }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_SIN:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_COS:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_339 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SIN, CFN_BUILT_IN_COS, CFN_BUILT_IN_TAN))
					      return true;
					  }
				        }
				    }
				  break;
				case CFN_BUILT_IN_TAN:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_340 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SIN, CFN_BUILT_IN_TAN, CFN_BUILT_IN_COS))
					      return true;
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
	      case CFN_BUILT_IN_TAN:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SIN:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_344 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TAN, CFN_BUILT_IN_SIN, CFN_BUILT_IN_COS))
					      return true;
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
	      case CFN_BUILT_IN_COSF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SINF:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_343 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COSF, CFN_BUILT_IN_SINF, CFN_BUILT_IN_TANF))
					      return true;
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
	      case CFN_BUILT_IN_COSL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SINL:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_343 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COSL, CFN_BUILT_IN_SINL, CFN_BUILT_IN_TANL))
					      return true;
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
	      case CFN_BUILT_IN_POWF:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    switch (TREE_CODE (_q21))
		      {
		      case REAL_CST:
		        {
			  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
				if (gimple_simplify_345 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POWF))
				  return true;
			      }
			    }
		          break;
		        }
		      default:;
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
		    switch (TREE_CODE (_q21))
		      {
		      case REAL_CST:
		        {
			  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
				if (gimple_simplify_345 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POWL))
				  return true;
			      }
			    }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_SINF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_COSF:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_339 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SINF, CFN_BUILT_IN_COSF, CFN_BUILT_IN_TANF))
					      return true;
					  }
				        }
				    }
				  break;
				case CFN_BUILT_IN_TANF:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_340 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SINF, CFN_BUILT_IN_TANF, CFN_BUILT_IN_COSF))
					      return true;
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
	      case CFN_BUILT_IN_SINH:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_COSH:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_341 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SINH, CFN_BUILT_IN_COSH, CFN_BUILT_IN_TANH))
					      return true;
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
	      case CFN_BUILT_IN_SINL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_COSL:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_339 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SINL, CFN_BUILT_IN_COSL, CFN_BUILT_IN_TANL))
					      return true;
					  }
				        }
				    }
				  break;
				case CFN_BUILT_IN_TANL:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_340 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SINL, CFN_BUILT_IN_TANL, CFN_BUILT_IN_COSL))
					      return true;
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
	      case CFN_BUILT_IN_TANF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SINF:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_344 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TANF, CFN_BUILT_IN_SINF, CFN_BUILT_IN_COSF))
					      return true;
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
	      case CFN_BUILT_IN_TANH:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SINH:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_342 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TANH, CFN_BUILT_IN_SINH, CFN_BUILT_IN_COSH))
					      return true;
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
	      case CFN_BUILT_IN_TANL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SINL:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_344 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TANL, CFN_BUILT_IN_SINL, CFN_BUILT_IN_COSL))
					      return true;
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
	      case CFN_COS:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_SIN:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_343 (res_op, seq, valueize, type, captures, CFN_COS, CFN_SIN, CFN_TAN))
					      return true;
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
	      case CFN_POW:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    switch (TREE_CODE (_q21))
		      {
		      case REAL_CST:
		        {
			  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
				if (gimple_simplify_345 (res_op, seq, valueize, type, captures, CFN_POW))
				  return true;
			      }
			    }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_SIN:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_COS:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_339 (res_op, seq, valueize, type, captures, CFN_SIN, CFN_COS, CFN_TAN))
					      return true;
					  }
				        }
				    }
				  break;
				case CFN_TAN:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_340 (res_op, seq, valueize, type, captures, CFN_SIN, CFN_TAN, CFN_COS))
					      return true;
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
	      case CFN_TAN:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_SIN:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_344 (res_op, seq, valueize, type, captures, CFN_TAN, CFN_SIN, CFN_COS))
					      return true;
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
	      case CFN_SINH:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_COSH:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_341 (res_op, seq, valueize, type, captures, CFN_SINH, CFN_COSH, CFN_TANH))
					      return true;
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
	      case CFN_TANH:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_SINH:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					    if (gimple_simplify_342 (res_op, seq, valueize, type, captures, CFN_TANH, CFN_SINH, CFN_COSH))
					      return true;
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
	      case CFN_BUILT_IN_POW:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		      if (gimple_simplify_346 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POW))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POWF:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		      if (gimple_simplify_346 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POWF))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POWL:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		      if (gimple_simplify_346 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POWL))
		        return true;
		    }
	          }
	        break;
	      case CFN_POW:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		      if (gimple_simplify_346 (res_op, seq, valueize, type, captures, CFN_POW))
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
  return false;
}

bool
gimple_simplify_GT_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
	      case RDIV_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q20))
		    {
		    case REAL_CST:
		      {
			if (real_zerop (_p1))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			      if (gimple_simplify_347 (res_op, seq, valueize, type, captures, GT_EXPR, LT_EXPR))
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
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
			if (gimple_simplify_348 (res_op, seq, valueize, type, captures, LT_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case MULT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case MULT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, GT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, GT_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, GT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, GT_EXPR))
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
	          break;
	        }
	      case PLUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (integer_minus_onep (_q21))
		    {
		      switch (TREE_CODE (_p1))
		        {
			case INTEGER_CST:
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			      if (gimple_simplify_374 (res_op, seq, valueize, type, captures, GT_EXPR, LE_EXPR))
			        return true;
			    }
			    break;
			  }
		        default:;
		        }
		    }
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case PLUS_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, GT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, GT_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, GT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		  switch (TREE_CODE (_q21))
		    {
		    case INTEGER_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    case PLUS_EXPR:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_assign_rhs2 (_a2);
					_q51 = do_valueize (valueize, _q51);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					      if (gimple_simplify_351 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		        break;
		      }
		    default:;
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, GT_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, GT_EXPR))
			  return true;
		      }
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
			      case EXACT_DIV_EXPR:
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
						    CASE_CONVERT:
						      {
							tree _q60 = gimple_assign_rhs1 (_a3);
							_q60 = do_valueize (valueize, _q60);
							switch (TREE_CODE (_q60))
							  {
							  case SSA_NAME:
							    if (gimple *_d4 = get_def (valueize, _q60))
							      {
								if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								  switch (gimple_assign_rhs_code (_a4))
								    {
								    case EXACT_DIV_EXPR:
								      {
									tree _q70 = gimple_assign_rhs1 (_a4);
									_q70 = do_valueize (valueize, _q70);
									tree _q71 = gimple_assign_rhs2 (_a4);
									_q71 = do_valueize (valueize, _q71);
									if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
									  {
									    {
									      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q70 };
									      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, GT_EXPR))
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
	      case EXACT_DIV_EXPR:
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
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    case EXACT_DIV_EXPR:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_assign_rhs2 (_a2);
					_q51 = do_valueize (valueize, _q51);
					if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					  {
					    {
					      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q50 };
					      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		        break;
		      }
		    default:;
		    }
	          break;
	        }
	      case TRUNC_DIV_EXPR:
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_220 (res_op, seq, valueize, type, captures, GT_EXPR))
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
	      case MINUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case MINUS_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					if (gimple_simplify_353 (res_op, seq, valueize, type, captures, GT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_354 (res_op, seq, valueize, type, captures, GT_EXPR))
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
	          break;
	        }
	      case POINTER_DIFF_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case POINTER_DIFF_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q50 };
					if (gimple_simplify_224 (res_op, seq, valueize, type, captures, GT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
					if (gimple_simplify_225 (res_op, seq, valueize, type, captures, GT_EXPR))
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
	      case PLUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q30, _p0 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, LT_EXPR))
			  return true;
		      }
		    }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, LT_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case MINUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, GT_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case MINUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, LT_EXPR))
			  return true;
		      }
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
		  if (tree_expr_nonnegative_p (_q20))
		    {
		      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, GT_EXPR, BIT_AND_EXPR))
			      return true;
			  }
		        }
		    }
		  if (tree_expr_nonnegative_p (_q21))
		    {
		      if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, GT_EXPR, BIT_AND_EXPR))
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case BIT_IOR_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      if (tree_expr_nonnegative_p (_q30))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, LT_EXPR, BIT_IOR_EXPR))
			      return true;
			  }
		        }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      if (tree_expr_nonnegative_p (_q31))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, LT_EXPR, BIT_IOR_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case MAX_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR, LT_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR, LT_EXPR))
			  return true;
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
				if (gimple_simplify_356 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR, BIT_IOR_EXPR))
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case MIN_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, LT_EXPR, GT_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, LT_EXPR, GT_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case MAX_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case MIN_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MIN_EXPR, LT_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MIN_EXPR, LT_EXPR))
			  return true;
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
				if (gimple_simplify_356 (res_op, seq, valueize, type, captures, MIN_EXPR, GT_EXPR, BIT_AND_EXPR))
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
 && (TREE_CODE_CLASS (GT_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2444;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2444;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2444;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2444:;
					  }
				      }
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
 && (TREE_CODE_CLASS (GT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2445;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2445;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2445;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2445:;
		      }
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
 && (TREE_CODE_CLASS (GT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2446;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2446;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2446;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2446:;
		      }
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
      if (gimple_simplify_375 (res_op, seq, valueize, type, captures, GT_EXPR, GE_EXPR))
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
			if (integer_zerop (_p1))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
			      if (gimple_simplify_253 (res_op, seq, valueize, type, captures, GT_EXPR, LT_EXPR))
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_306 (res_op, seq, valueize, type, captures, GT_EXPR))
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
	      case BIT_NOT_EXPR:
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
			      case BIT_NOT_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _p1, _q40 };
				    if (gimple_simplify_257 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		{
		  tree _p1_pops[1];
		  if (gimple_nop_convert (_p1, _p1_pops, valueize))
		    {
		      tree _q40 = _p1_pops[0];
		      switch (TREE_CODE (_q40))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q40))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  case BIT_NOT_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q40, _q50 };
					if (gimple_simplify_257 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		}
		if (CONSTANT_CLASS_P (_p1))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      if (gimple_simplify_258 (res_op, seq, valueize, type, captures, GT_EXPR, LT_EXPR))
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
		  case BIT_NOT_EXPR:
		    {
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      switch (TREE_CODE (_p1))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _p1))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  case BIT_NOT_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _p1, _q50 };
					if (gimple_simplify_259 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		    {
		      tree _p1_pops[1];
		      if (gimple_nop_convert (_p1, _p1_pops, valueize))
		        {
			  tree _q50 = _p1_pops[0];
			  switch (TREE_CODE (_q50))
			    {
			    case SSA_NAME:
			      if (gimple *_d2 = get_def (valueize, _q50))
			        {
				  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				    switch (gimple_assign_rhs_code (_a2))
				      {
				      case BIT_NOT_EXPR:
				        {
					  tree _q60 = gimple_assign_rhs1 (_a2);
					  _q60 = do_valueize (valueize, _q60);
					  {
					    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q50, _q60 };
					    if (gimple_simplify_259 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		    }
		    if (CONSTANT_CLASS_P (_p1))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
			  if (gimple_simplify_258 (res_op, seq, valueize, type, captures, GT_EXPR, LT_EXPR))
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
    }
}
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_260 (res_op, seq, valueize, type, captures, GT_EXPR))
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
	      case PLUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, PLUS_EXPR, GT_EXPR))
				  return true;
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
			      if (gimple_simplify_359 (res_op, seq, valueize, type, captures, GT_EXPR, LE_EXPR))
			        return true;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, MINUS_EXPR, GT_EXPR))
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
				if (gimple_simplify_262 (res_op, seq, valueize, type, captures, GT_EXPR))
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, GT_EXPR, GT_EXPR))
				      return true;
				  }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    case REAL_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			  if (gimple_simplify_266 (res_op, seq, valueize, type, captures, GT_EXPR, GE_EXPR))
			    return true;
			}
		        break;
		      }
		    default:;
		    }
	          break;
	        }
	      case EXACT_DIV_EXPR:
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
				if (gimple_simplify_360 (res_op, seq, valueize, type, captures, GT_EXPR))
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
			      case EXACT_DIV_EXPR:
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
					  case INTEGER_CST:
					    {
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
						if (gimple_simplify_361 (res_op, seq, valueize, type, captures, GT_EXPR, LT_EXPR))
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
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
				    if (gimple_simplify_270 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
		    if (gimple_simplify_271 (res_op, seq, valueize, type, captures, GT_EXPR))
		      return true;
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_376 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR))
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, GT_EXPR, LT_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, GT_EXPR, LT_EXPR))
		        return true;
		    }
		  }
	          break;
	        }
	      case COND_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  tree _q22 = gimple_assign_rhs3 (_a1);
		  _q22 = do_valueize (valueize, _q22);
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _p1 };
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, GT_EXPR))
		      return true;
		  }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_SQRTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SQRTF:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, GT_EXPR))
					  return true;
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
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, GT_EXPR))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_SQRTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SQRTL:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, GT_EXPR))
					  return true;
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
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, GT_EXPR))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_SQRT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_SQRT:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, GT_EXPR))
					  return true;
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
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, GT_EXPR))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_SQRT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_SQRT:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_SQRT, GT_EXPR))
					  return true;
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
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_SQRT, GT_EXPR))
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
	      case COND_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  tree _q32 = gimple_assign_rhs3 (_a1);
		  _q32 = do_valueize (valueize, _q32);
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q32, _p0 };
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, LT_EXPR))
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
			      case ADDR_EXPR:
			        {
				  switch (TREE_CODE (_p1))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _p1))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q40 = gimple_assign_rhs1 (_a3);
						  _q40 = do_valueize (valueize, _q40);
						  switch (TREE_CODE (_q40))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q40))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case ADDR_EXPR:
							        {
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
								    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GT_EXPR))
								      return true;
								  }
							          break;
							        }
							      default:;
							      }
						        }
						      break;
						    case ADDR_EXPR:
						      {
							{
							  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GT_EXPR))
							    return true;
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
						    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GT_EXPR))
						      return true;
						  }
					          break;
					        }
					      default:;
					      }
				        }
				      break;
				    case ADDR_EXPR:
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		        }
		      break;
		    case ADDR_EXPR:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    CASE_CONVERT:
				      {
					tree _q40 = gimple_assign_rhs1 (_a2);
					_q40 = do_valueize (valueize, _q40);
					switch (TREE_CODE (_q40))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _q40))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
						    {
						    case ADDR_EXPR:
						      {
							{
							  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GT_EXPR))
							    return true;
							}
						        break;
						      }
						    default:;
						    }
					      }
					    break;
					  case ADDR_EXPR:
					    {
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
						if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GT_EXPR))
						  return true;
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GT_EXPR))
					    return true;
					}
				        break;
				      }
				    default:;
				    }
			      }
			    break;
			  case ADDR_EXPR:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GT_EXPR))
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
		  if (uniform_integer_cst_p (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			if (gimple_simplify_362 (res_op, seq, valueize, type, captures, GT_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case ADDR_EXPR:
	        {
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
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
					      case ADDR_EXPR:
					        {
						  {
						    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q30 };
						    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GT_EXPR))
						      return true;
						  }
					          break;
					        }
					      default:;
					      }
				        }
				      break;
				    case ADDR_EXPR:
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q30 };
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GT_EXPR))
					    return true;
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
				    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GT_EXPR))
				      return true;
				  }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    case ADDR_EXPR:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _p1 };
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GT_EXPR))
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
        }
      break;
    case ADDR_EXPR:
      {
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
			switch (TREE_CODE (_q30))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _q30))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    case ADDR_EXPR:
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q30 };
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GT_EXPR))
					    return true;
					}
				        break;
				      }
				    default:;
				    }
			      }
			    break;
			  case ADDR_EXPR:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q30 };
				if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GT_EXPR))
				  return true;
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GT_EXPR))
			    return true;
			}
		        break;
		      }
		    default:;
		    }
	      }
	    break;
	  case ADDR_EXPR:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _p1 };
		if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GT_EXPR))
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
  if (uniform_integer_cst_p (_p1))
    {
      {
	tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _p1 };
	if (gimple_simplify_363 (res_op, seq, valueize, type, captures, GT_EXPR))
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
			      case NE_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (tree_swap_operands_p (_q30, _q31))
				    std::swap (_q30, _q31);
				  if (integer_zerop (_q31))
				    {
				      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
					{
					  {
					    tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					    if (TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2447;
						{
						  tree tem;
						  tem =  constant_boolean_node (false, type);
						  res_op->set_value (tem);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1107, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2447:;
					      }
					  }
				        }
				    }
			          break;
			        }
			      case PLUS_EXPR:
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
					      CASE_CONVERT:
					        {
						  tree _q40 = gimple_assign_rhs1 (_a3);
						  _q40 = do_valueize (valueize, _q40);
						  switch (TREE_CODE (_q31))
						    {
						    case INTEGER_CST:
						      {
							if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
							  {
							    {
							      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q31 };
							      if (gimple_simplify_364 (res_op, seq, valueize, type, captures, GT_EXPR, LE_EXPR))
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
		  switch (TREE_CODE (_q30))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q30))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case PLUS_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  switch (TREE_CODE (_q40))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q40))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
						    {
						      switch (TREE_CODE (_q41))
						        {
							case INTEGER_CST:
							  {
							    {
							      tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _p0, _q41 };
							      if (gimple_simplify_364 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
							        return true;
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
	      case PLUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      switch (TREE_CODE (_q31))
		        {
			case INTEGER_CST:
			  {
			    {
			      tree captures[5] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _p0, _q31 };
			      if (gimple_simplify_359 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
			        return true;
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
	      case MINUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_365 (res_op, seq, valueize, type, captures, GT_EXPR))
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
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case PLUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  switch (TREE_CODE (_q30))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q30))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case MINUS_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
				    {
				      if (integer_minus_onep (_q31))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
					    if (gimple_simplify_377 (res_op, seq, valueize, type, captures, LT_EXPR))
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
	      case REALPART_EXPR:
	        {
		  tree _q20 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q20) == SSA_NAME
		       || is_gimple_min_invariant (_q20)))
		    {
		      _q20 = do_valueize (valueize, _q20);
		      switch (TREE_CODE (_q20))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q20))
			    {
			      if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			        switch (gimple_call_combined_fn (_c2))
				  {
				  case CFN_SUB_OVERFLOW:
				    if (gimple_call_num_args (_c2) == 2)
				      {
					tree _q30 = gimple_call_arg (_c2, 0);
					_q30 = do_valueize (valueize, _q30);
					tree _q31 = gimple_call_arg (_c2, 1);
					_q31 = do_valueize (valueize, _q31);
					if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
					      if (gimple_simplify_378 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR))
					        return true;
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case REALPART_EXPR:
	        {
		  tree _q30 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q30) == SSA_NAME
		       || is_gimple_min_invariant (_q30)))
		    {
		      _q30 = do_valueize (valueize, _q30);
		      switch (TREE_CODE (_q30))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q30))
			    {
			      if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			        switch (gimple_call_combined_fn (_c2))
				  {
				  case CFN_ADD_OVERFLOW:
				    if (gimple_call_num_args (_c2) == 2)
				      {
					tree _q40 = gimple_call_arg (_c2, 0);
					_q40 = do_valueize (valueize, _q40);
					tree _q41 = gimple_call_arg (_c2, 1);
					_q41 = do_valueize (valueize, _q41);
					if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q41 };
					      if (gimple_simplify_379 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR))
					        return true;
					    }
					  }
					if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q40 };
					      if (gimple_simplify_379 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR))
					        return true;
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
	      case TRUNC_DIV_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (integer_all_onesp (_q30))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
			if (gimple_simplify_380 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
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
		    case REAL_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case REAL_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_370 (res_op, seq, valueize, type, captures, GT_EXPR, LT_EXPR))
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
	      case PLUS_EXPR:
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_371 (res_op, seq, valueize, type, captures, PLUS_EXPR, MINUS_EXPR, GT_EXPR, LT_EXPR))
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
	      case MINUS_EXPR:
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_371 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR, GT_EXPR, LT_EXPR))
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
				if (gimple_simplify_296 (res_op, seq, valueize, type, captures, GT_EXPR, LT_EXPR))
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
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_CTZ:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GT_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZ))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_FFS:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_FFS))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_FFSIMAX:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_FFSIMAX))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_POPCOUNT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    if (integer_zerop (_p1))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNT))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_CTZL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GT_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZL))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_FFSL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_FFSL))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_POPCOUNTLL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    if (integer_zerop (_p1))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTLL))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_CTZIMAX:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GT_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZIMAX))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_POPCOUNT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    if (integer_zerop (_p1))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, CFN_POPCOUNT))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_CTZ:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
			    if (gimple_simplify_373 (res_op, seq, valueize, type, captures, GT_EXPR, EQ_EXPR))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GT_EXPR, EQ_EXPR, CFN_CTZ))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_FFS:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_FFS))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_POPCOUNTIMAX:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    if (integer_zerop (_p1))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTIMAX))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_POPCOUNTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    if (integer_zerop (_p1))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTL))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_CTZLL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GT_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZLL))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_FFSLL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_FFSLL))
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
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_HYPOTL (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn hypots = CFN_BUILT_IN_HYPOTL;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2565;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1132, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2565:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn hypots = CFN_BUILT_IN_HYPOTL;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2566;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1132, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2566:;
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
		    const combined_fn hypots = CFN_BUILT_IN_HYPOTL;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2567;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1133, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2567:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		    const combined_fn hypots = CFN_BUILT_IN_HYPOTL;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2568;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1133, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2568:;
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
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_BUILT_IN_HYPOTL))
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
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_BUILT_IN_HYPOTL))
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
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_BUILT_IN_HYPOTL))
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
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_BUILT_IN_HYPOTL))
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
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_BUILT_IN_HYPOTL))
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
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_BUILT_IN_HYPOTL))
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
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_BUILT_IN_HYPOTL))
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
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_BUILT_IN_HYPOTL))
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
      if (gimple_simplify_418 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_HYPOTL))
        return true;
    }
  }
  if (real_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
	if (gimple_simplify_418 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_HYPOTL))
	  return true;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_419 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_HYPOTL))
	  return true;
      }
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMINL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINL))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINL))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINL))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_506 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINL))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2610;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMAXL, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2610;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2610:;
				      }
				  }
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
gimple_simplify_CFN_BUILT_IN_FMINF128 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF128))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF128))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF128))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_506 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF128))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2614;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMAXF128, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2614;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2614:;
				      }
				  }
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
gimple_simplify_CFN_FMIN (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_FMIN))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_FMIN))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_FMIN))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_506 (res_op, seq, valueize, type, captures, CFN_FMIN))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2618;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_FMAX, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2618;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2618:;
				      }
				  }
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
gimple_simplify_CFN_BUILT_IN_FMAXL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXL))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXL))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXL))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_507 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXL))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2621;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMINL, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2621;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2621:;
				      }
				  }
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
gimple_simplify_CFN_BUILT_IN_FMAXF128 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF128))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF128))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF128))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_507 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF128))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2625;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMINF128, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2625;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2625:;
				      }
				  }
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
gimple_simplify_CFN_FMAX (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_FMAX))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_FMAX))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_FMAX))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_507 (res_op, seq, valueize, type, captures, CFN_FMAX))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2629;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_FMIN, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2629;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2629:;
				      }
				  }
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
gimple_simplify_UNLT_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
 && (TREE_CODE_CLASS (UNLT_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2642;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), UNLT_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2642;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), UNLT_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2642;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2642:;
					  }
				      }
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
 && (TREE_CODE_CLASS (UNLT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2643;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNLT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2643;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNLT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2643;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2643:;
		      }
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
 && (TREE_CODE_CLASS (UNLT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2644;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNLT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2644;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNLT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2644;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2644:;
		      }
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_615 (res_op, seq, valueize, type, captures, UNLT_EXPR))
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, UNLT_EXPR, LT_EXPR))
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, UNLT_EXPR, UNGT_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, UNLT_EXPR, UNGT_EXPR))
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
	  if (gimple_simplify_614 (res_op, seq, valueize, type, captures, UNLT_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify_LTGT_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
 && (TREE_CODE_CLASS (LTGT_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2657;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), LTGT_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2657;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), LTGT_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2657;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2657:;
					  }
				      }
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
 && (TREE_CODE_CLASS (LTGT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2658;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LTGT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2658;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LTGT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2658;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2658:;
		      }
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
 && (TREE_CODE_CLASS (LTGT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2659;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), LTGT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2659;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LTGT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2659;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2659:;
		      }
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (!flag_trapping_math || !tree_expr_maybe_nan_p (captures[0])
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2660;
	    {
	      tree tem;
	      tem =  constant_boolean_node (false, type);
	      res_op->set_value (tem);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1159, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2660:;
	  }
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, LTGT_EXPR, NE_EXPR))
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, LTGT_EXPR, LTGT_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, LTGT_EXPR, LTGT_EXPR))
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
	  if (gimple_simplify_614 (res_op, seq, valueize, type, captures, LTGT_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify_BIT_INSERT_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2)
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
	      case BIT_FIELD_REF:
	        {
		  tree _q30 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q30) == SSA_NAME
		       || is_gimple_min_invariant (_q30)))
		    {
		      _q30 = do_valueize (valueize, _q30);
		      tree _q31 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 1);
		      if ((TREE_CODE (_q31) == SSA_NAME
		           || is_gimple_min_invariant (_q31)))
		        {
			  _q31 = do_valueize (valueize, _q31);
			  tree _q32 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 2);
			  if ((TREE_CODE (_q32) == SSA_NAME
			       || is_gimple_min_invariant (_q32)))
			    {
			      _q32 = do_valueize (valueize, _q32);
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
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2885;
					    {
					      res_op->set_op (VEC_PERM_EXPR, type, 3);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->ops[2] =  vec_perm_indices_to_tree
 (build_vector_type (ssizetype, nunits), sel);
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1219, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2885:;
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMAF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2)
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
		    if (gimple_simplify_641 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAF))
		      return true;
		  }
		  switch (TREE_CODE (_p2))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p2))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q50 };
				    if (gimple_simplify_642 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAF))
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _p2 };
		    if (gimple_simplify_641 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAF))
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
  switch (TREE_CODE (_p2))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p2))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q40 };
		    if (gimple_simplify_643 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAF))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  switch (TREE_CODE (_p2))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p2))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q50 };
				    if (gimple_simplify_642 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAF))
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
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMA (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2)
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
		    if (gimple_simplify_641 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMA))
		      return true;
		  }
		  switch (TREE_CODE (_p2))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p2))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q50 };
				    if (gimple_simplify_642 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMA))
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _p2 };
		    if (gimple_simplify_641 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMA))
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
  switch (TREE_CODE (_p2))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p2))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q40 };
		    if (gimple_simplify_643 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMA))
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  switch (TREE_CODE (_p2))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p2))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q50 };
				    if (gimple_simplify_642 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMA))
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
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_FMS (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2)
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
		    if (gimple_simplify_644 (res_op, seq, valueize, type, captures))
		      return true;
		  }
		  switch (TREE_CODE (_p2))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p2))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q50 };
				    if (gimple_simplify_645 (res_op, seq, valueize, type, captures))
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _p2 };
		    if (gimple_simplify_644 (res_op, seq, valueize, type, captures))
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
  switch (TREE_CODE (_p2))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p2))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q40 };
		    if (canonicalize_math_after_vectorization_p ()
)
		      {
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2886;
			{
			  res_op->set_op (CFN_FMA, type, 3);
			  res_op->ops[0] = captures[0];
			  res_op->ops[1] = captures[1];
			  res_op->ops[2] = captures[2];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1220, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2886:;
		      }
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
		  switch (TREE_CODE (_p2))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p2))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q50 };
				    if (gimple_simplify_645 (res_op, seq, valueize, type, captures))
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
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_VCOND_MASK_LEN (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p2))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p2))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VIEW_CONVERT_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q40) == SSA_NAME
		       || is_gimple_min_invariant (_q40)))
		    {
		      _q40 = do_valueize (valueize, _q40);
		      switch (TREE_CODE (_q40))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q40))
			    {
			      if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			        switch (gimple_call_combined_fn (_c2))
				  {
				  case CFN_FMA:
				    if (gimple_call_num_args (_c2) == 5)
				      {
					tree _q50 = gimple_call_arg (_c2, 0);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_call_arg (_c2, 1);
					_q51 = do_valueize (valueize, _q51);
					tree _q52 = gimple_call_arg (_c2, 2);
					_q52 = do_valueize (valueize, _q52);
					tree _q53 = gimple_call_arg (_c2, 3);
					_q53 = do_valueize (valueize, _q53);
					tree _q54 = gimple_call_arg (_c2, 4);
					_q54 = do_valueize (valueize, _q54);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					{
					  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _q52, _q53, _q54 };
					  if (gimple_simplify_670 (res_op, seq, valueize, type, captures, CFN_FMA, CFN_COND_LEN_FMA))
					    return true;
					}
				      }
				    break;
				  case CFN_FMS:
				    if (gimple_call_num_args (_c2) == 5)
				      {
					tree _q50 = gimple_call_arg (_c2, 0);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_call_arg (_c2, 1);
					_q51 = do_valueize (valueize, _q51);
					tree _q52 = gimple_call_arg (_c2, 2);
					_q52 = do_valueize (valueize, _q52);
					tree _q53 = gimple_call_arg (_c2, 3);
					_q53 = do_valueize (valueize, _q53);
					tree _q54 = gimple_call_arg (_c2, 4);
					_q54 = do_valueize (valueize, _q54);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					{
					  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _q52, _q53, _q54 };
					  if (gimple_simplify_670 (res_op, seq, valueize, type, captures, CFN_FMS, CFN_COND_LEN_FMS))
					    return true;
					}
				      }
				    break;
				  case CFN_FNMA:
				    if (gimple_call_num_args (_c2) == 5)
				      {
					tree _q50 = gimple_call_arg (_c2, 0);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_call_arg (_c2, 1);
					_q51 = do_valueize (valueize, _q51);
					tree _q52 = gimple_call_arg (_c2, 2);
					_q52 = do_valueize (valueize, _q52);
					tree _q53 = gimple_call_arg (_c2, 3);
					_q53 = do_valueize (valueize, _q53);
					tree _q54 = gimple_call_arg (_c2, 4);
					_q54 = do_valueize (valueize, _q54);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					{
					  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _q52, _q53, _q54 };
					  if (gimple_simplify_670 (res_op, seq, valueize, type, captures, CFN_FNMA, CFN_COND_LEN_FNMA))
					    return true;
					}
				      }
				    break;
				  case CFN_FNMS:
				    if (gimple_call_num_args (_c2) == 5)
				      {
					tree _q50 = gimple_call_arg (_c2, 0);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_call_arg (_c2, 1);
					_q51 = do_valueize (valueize, _q51);
					tree _q52 = gimple_call_arg (_c2, 2);
					_q52 = do_valueize (valueize, _q52);
					tree _q53 = gimple_call_arg (_c2, 3);
					_q53 = do_valueize (valueize, _q53);
					tree _q54 = gimple_call_arg (_c2, 4);
					_q54 = do_valueize (valueize, _q54);
					if (tree_swap_operands_p (_q50, _q51))
					  std::swap (_q50, _q51);
					{
					  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _q52, _q53, _q54 };
					  if (gimple_simplify_670 (res_op, seq, valueize, type, captures, CFN_FNMS, CFN_COND_LEN_FNMS))
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
		    }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_FMA:
	        if (gimple_call_num_args (_c1) == 5)
	          {
		    tree _q40 = gimple_call_arg (_c1, 0);
		    _q40 = do_valueize (valueize, _q40);
		    tree _q41 = gimple_call_arg (_c1, 1);
		    _q41 = do_valueize (valueize, _q41);
		    tree _q42 = gimple_call_arg (_c1, 2);
		    _q42 = do_valueize (valueize, _q42);
		    tree _q43 = gimple_call_arg (_c1, 3);
		    _q43 = do_valueize (valueize, _q43);
		    tree _q44 = gimple_call_arg (_c1, 4);
		    _q44 = do_valueize (valueize, _q44);
		    if (tree_swap_operands_p (_q40, _q41))
		      std::swap (_q40, _q41);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _q42, _q43, _q44 };
		      if (gimple_simplify_670 (res_op, seq, valueize, type, captures, CFN_FMA, CFN_COND_LEN_FMA))
		        return true;
		    }
	          }
	        break;
	      case CFN_FMS:
	        if (gimple_call_num_args (_c1) == 5)
	          {
		    tree _q40 = gimple_call_arg (_c1, 0);
		    _q40 = do_valueize (valueize, _q40);
		    tree _q41 = gimple_call_arg (_c1, 1);
		    _q41 = do_valueize (valueize, _q41);
		    tree _q42 = gimple_call_arg (_c1, 2);
		    _q42 = do_valueize (valueize, _q42);
		    tree _q43 = gimple_call_arg (_c1, 3);
		    _q43 = do_valueize (valueize, _q43);
		    tree _q44 = gimple_call_arg (_c1, 4);
		    _q44 = do_valueize (valueize, _q44);
		    if (tree_swap_operands_p (_q40, _q41))
		      std::swap (_q40, _q41);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _q42, _q43, _q44 };
		      if (gimple_simplify_670 (res_op, seq, valueize, type, captures, CFN_FMS, CFN_COND_LEN_FMS))
		        return true;
		    }
	          }
	        break;
	      case CFN_FNMA:
	        if (gimple_call_num_args (_c1) == 5)
	          {
		    tree _q40 = gimple_call_arg (_c1, 0);
		    _q40 = do_valueize (valueize, _q40);
		    tree _q41 = gimple_call_arg (_c1, 1);
		    _q41 = do_valueize (valueize, _q41);
		    tree _q42 = gimple_call_arg (_c1, 2);
		    _q42 = do_valueize (valueize, _q42);
		    tree _q43 = gimple_call_arg (_c1, 3);
		    _q43 = do_valueize (valueize, _q43);
		    tree _q44 = gimple_call_arg (_c1, 4);
		    _q44 = do_valueize (valueize, _q44);
		    if (tree_swap_operands_p (_q40, _q41))
		      std::swap (_q40, _q41);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _q42, _q43, _q44 };
		      if (gimple_simplify_670 (res_op, seq, valueize, type, captures, CFN_FNMA, CFN_COND_LEN_FNMA))
		        return true;
		    }
	          }
	        break;
	      case CFN_FNMS:
	        if (gimple_call_num_args (_c1) == 5)
	          {
		    tree _q40 = gimple_call_arg (_c1, 0);
		    _q40 = do_valueize (valueize, _q40);
		    tree _q41 = gimple_call_arg (_c1, 1);
		    _q41 = do_valueize (valueize, _q41);
		    tree _q42 = gimple_call_arg (_c1, 2);
		    _q42 = do_valueize (valueize, _q42);
		    tree _q43 = gimple_call_arg (_c1, 3);
		    _q43 = do_valueize (valueize, _q43);
		    tree _q44 = gimple_call_arg (_c1, 4);
		    _q44 = do_valueize (valueize, _q44);
		    if (tree_swap_operands_p (_q40, _q41))
		      std::swap (_q40, _q41);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _q42, _q43, _q44 };
		      if (gimple_simplify_670 (res_op, seq, valueize, type, captures, CFN_FNMS, CFN_COND_LEN_FNMS))
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
  return false;
}

bool
gimple_simplify_CFN_COND_ADD (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2, tree _p3)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p2))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p2))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VEC_COND_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  tree _q42 = gimple_assign_rhs3 (_a1);
		  _q42 = do_valueize (valueize, _q42);
		  if (zerop (_q42))
		    {
		      if ((_p3 == _p1 && ! TREE_SIDE_EFFECTS (_p3)) || (operand_equal_p (_p3, _p1, 0) && types_match (_p3, _p1)))
			{
			  {
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42 };
			    if (ANY_INTEGRAL_TYPE_P (type)
 || (FLOAT_TYPE_P (type)
 && fold_real_zero_addition_p (type, NULL_TREE, captures[4], 0))
)
			      {
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2904;
				{
				  res_op->set_op (CFN_COND_ADD, type, 4);
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[2];
				    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    tem_op.resimplify (lseq, valueize);
				    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r1) goto next_after_fail2904;
				    res_op->ops[0] = _r1;
				  }
				  res_op->ops[1] = captures[1];
				  res_op->ops[2] = captures[3];
				  res_op->ops[3] = captures[1];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1236, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2904:;
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
  return false;
}

bool
gimple_simplify (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper code, const tree type, tree _p0, tree _p1, tree _p2, tree _p3)
{
  switch (code.get_rep())
    {
    case -CFN_COND_MUL:
      return gimple_simplify_CFN_COND_MUL (res_op, seq, valueize, code, type, _p0, _p1, _p2, _p3);
    case -CFN_COND_ADD:
      return gimple_simplify_CFN_COND_ADD (res_op, seq, valueize, code, type, _p0, _p1, _p2, _p3);
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_VCOND_MASK_LEN (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2, tree _p3, tree _p4)
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
	      case VIEW_CONVERT_EXPR:
	        {
		  tree _q30 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q30) == SSA_NAME
		       || is_gimple_min_invariant (_q30)))
		    {
		      _q30 = do_valueize (valueize, _q30);
		      switch (TREE_CODE (_q30))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q30))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  case NEGATE_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_NEG), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2905;
						{
						  res_op->set_op (CFN_COND_LEN_NEG, type, 5);
						  res_op->ops[0] = captures[0];
						  {
						    tree _o1[1], _r1;
						    _o1[0] = captures[2];
						    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2905;
						    res_op->ops[1] = _r1;
						  }
						  res_op->ops[2] = captures[3];
						  res_op->ops[3] = captures[4];
						  res_op->ops[4] = captures[5];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1237, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2905:;
					      }
					}
				      }
				      break;
				    }
				  case BIT_NOT_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_NOT), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2906;
						{
						  res_op->set_op (CFN_COND_LEN_NOT, type, 5);
						  res_op->ops[0] = captures[0];
						  {
						    tree _o1[1], _r1;
						    _o1[0] = captures[2];
						    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2906;
						    res_op->ops[1] = _r1;
						  }
						  res_op->ops[2] = captures[3];
						  res_op->ops[3] = captures[4];
						  res_op->ops[4] = captures[5];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1237, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2906:;
					      }
					}
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_NEG), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2907;
			    {
			      res_op->set_op (CFN_COND_LEN_NEG, type, 5);
			      res_op->ops[0] = captures[0];
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2907;
				res_op->ops[1] = _r1;
			      }
			      res_op->ops[2] = captures[3];
			      res_op->ops[3] = captures[4];
			      res_op->ops[4] = captures[5];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1237, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2907:;
			  }
		    }
		  }
	          break;
	        }
	      case BIT_NOT_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_NOT), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2908;
			    {
			      res_op->set_op (CFN_COND_LEN_NOT, type, 5);
			      res_op->ops[0] = captures[0];
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[2];
				gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2908;
				res_op->ops[1] = _r1;
			      }
			      res_op->ops[2] = captures[3];
			      res_op->ops[3] = captures[4];
			      res_op->ops[4] = captures[5];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1237, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2908:;
			  }
		    }
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p2))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p2))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VIEW_CONVERT_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q40) == SSA_NAME
		       || is_gimple_min_invariant (_q40)))
		    {
		      _q40 = do_valueize (valueize, _q40);
		      switch (TREE_CODE (_q40))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q40))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  case NEGATE_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      {
					tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_NEG), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2909;
						{
						  res_op->set_op (CFN_COND_LEN_NEG, type, 5);
						  {
						    tree _o1[1], _r1;
						    _o1[0] = captures[0];
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2909;
						    res_op->ops[0] = _r1;
						  }
						  {
						    tree _o1[1], _r1;
						    _o1[0] = captures[3];
						    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2909;
						    res_op->ops[1] = _r1;
						  }
						  res_op->ops[2] = captures[1];
						  res_op->ops[3] = captures[4];
						  res_op->ops[4] = captures[5];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1238, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2909:;
					      }
					}
				      }
				      break;
				    }
				  case BIT_NOT_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      {
					tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_NOT), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2910;
						{
						  res_op->set_op (CFN_COND_LEN_NOT, type, 5);
						  {
						    tree _o1[1], _r1;
						    _o1[0] = captures[0];
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2910;
						    res_op->ops[0] = _r1;
						  }
						  {
						    tree _o1[1], _r1;
						    _o1[0] = captures[3];
						    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2910;
						    res_op->ops[1] = _r1;
						  }
						  res_op->ops[2] = captures[1];
						  res_op->ops[3] = captures[4];
						  res_op->ops[4] = captures[5];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1238, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2910:;
					      }
					}
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
	      case NEGATE_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_NEG), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2911;
			    {
			      res_op->set_op (CFN_COND_LEN_NEG, type, 5);
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[0];
				gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2911;
				res_op->ops[0] = _r1;
			      }
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[3];
				gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2911;
				res_op->ops[1] = _r1;
			      }
			      res_op->ops[2] = captures[1];
			      res_op->ops[3] = captures[4];
			      res_op->ops[4] = captures[5];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1238, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2911:;
			  }
		    }
		  }
	          break;
	        }
	      case BIT_NOT_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_NOT), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2912;
			    {
			      res_op->set_op (CFN_COND_LEN_NOT, type, 5);
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[0];
				gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2912;
				res_op->ops[0] = _r1;
			      }
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[3];
				gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2912;
				res_op->ops[1] = _r1;
			      }
			      res_op->ops[2] = captures[1];
			      res_op->ops[3] = captures[4];
			      res_op->ops[4] = captures[5];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1238, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2912:;
			  }
		    }
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
	      case VIEW_CONVERT_EXPR:
	        {
		  tree _q30 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q30) == SSA_NAME
		       || is_gimple_min_invariant (_q30)))
		    {
		      _q30 = do_valueize (valueize, _q30);
		      switch (TREE_CODE (_q30))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q30))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  case PLUS_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      if (tree_swap_operands_p (_q40, _q41))
				        std::swap (_q40, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_ADD), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2913;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2913;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_ADD, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2913;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2913:;
					      }
					}
				      }
				      break;
				    }
				  case MINUS_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SUB), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2914;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2914;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SUB, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2914;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2914:;
					      }
					}
				      }
				      break;
				    }
				  case MULT_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      if (tree_swap_operands_p (_q40, _q41))
				        std::swap (_q40, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MUL), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2915;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2915;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MUL, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2915;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2915:;
					      }
					}
				      }
				      break;
				    }
				  case TRUNC_DIV_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_DIV), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2916;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2916;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_DIV, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2916;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2916:;
					      }
					}
				      }
				      break;
				    }
				  case TRUNC_MOD_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MOD), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2917;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2917;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MOD, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2917;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2917:;
					      }
					}
				      }
				      break;
				    }
				  case RDIV_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_RDIV), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2918;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2918;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_RDIV, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2918;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2918:;
					      }
					}
				      }
				      break;
				    }
				  case MIN_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      if (tree_swap_operands_p (_q40, _q41))
				        std::swap (_q40, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MIN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2919;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2919;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MIN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2919;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2919:;
					      }
					}
				      }
				      break;
				    }
				  case MAX_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      if (tree_swap_operands_p (_q40, _q41))
				        std::swap (_q40, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MAX), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2920;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2920;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MAX, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2920;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2920:;
					      }
					}
				      }
				      break;
				    }
				  case BIT_AND_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      if (tree_swap_operands_p (_q40, _q41))
				        std::swap (_q40, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_AND), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2921;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2921;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_AND, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2921;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2921:;
					      }
					}
				      }
				      break;
				    }
				  case BIT_IOR_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      if (tree_swap_operands_p (_q40, _q41))
				        std::swap (_q40, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_IOR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2922;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2922;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_IOR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2922;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2922:;
					      }
					}
				      }
				      break;
				    }
				  case BIT_XOR_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      if (tree_swap_operands_p (_q40, _q41))
				        std::swap (_q40, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_XOR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2923;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2923;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_XOR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2923;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2923:;
					      }
					}
				      }
				      break;
				    }
				  case LSHIFT_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SHL), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2924;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2924;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SHL, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2924;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2924:;
					      }
					}
				      }
				      break;
				    }
				  case RSHIFT_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[1]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SHR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2925;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    _o1[0] = captures[0];
						    _o1[1] = captures[2];
						    _o1[2] = captures[3];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[4];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2925;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SHR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2925;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2925:;
					      }
					}
				      }
				      break;
				    }
				  default:;
				  }
			      else if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			        switch (gimple_call_combined_fn (_c2))
				  {
				  case CFN_FMAX:
				    if (gimple_call_num_args (_c2) == 2)
				      {
					tree _q40 = gimple_call_arg (_c2, 0);
					_q40 = do_valueize (valueize, _q40);
					tree _q41 = gimple_call_arg (_c2, 1);
					_q41 = do_valueize (valueize, _q41);
					{
					  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					  {
 tree op_type = TREE_TYPE (captures[1]);
					      if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_FMAX), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2926;
						  {
						    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						    {
						      tree _o1[6], _r1;
						      _o1[0] = captures[0];
						      _o1[1] = captures[2];
						      _o1[2] = captures[3];
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[4];
							gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
							tem_op.resimplify (lseq, valueize);
							_r2 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r2) goto next_after_fail2926;
							_o1[3] = _r2;
						      }
						      _o1[4] = captures[5];
						      _o1[5] = captures[6];
						      gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_FMAX, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2926;
						      res_op->ops[0] = _r1;
						    }
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2926:;
						}
					  }
					}
				      }
				    break;
				  case CFN_FMIN:
				    if (gimple_call_num_args (_c2) == 2)
				      {
					tree _q40 = gimple_call_arg (_c2, 0);
					_q40 = do_valueize (valueize, _q40);
					tree _q41 = gimple_call_arg (_c2, 1);
					_q41 = do_valueize (valueize, _q41);
					{
					  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					  {
 tree op_type = TREE_TYPE (captures[1]);
					      if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_FMIN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2927;
						  {
						    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						    {
						      tree _o1[6], _r1;
						      _o1[0] = captures[0];
						      _o1[1] = captures[2];
						      _o1[2] = captures[3];
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[4];
							gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
							tem_op.resimplify (lseq, valueize);
							_r2 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r2) goto next_after_fail2927;
							_o1[3] = _r2;
						      }
						      _o1[4] = captures[5];
						      _o1[5] = captures[6];
						      gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_FMIN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2927;
						      res_op->ops[0] = _r1;
						    }
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2927:;
						}
					  }
					}
				      }
				    break;
				  case CFN_COPYSIGN:
				    if (gimple_call_num_args (_c2) == 2)
				      {
					tree _q40 = gimple_call_arg (_c2, 0);
					_q40 = do_valueize (valueize, _q40);
					tree _q41 = gimple_call_arg (_c2, 1);
					_q41 = do_valueize (valueize, _q41);
					{
					  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _p2, _p3, _p4 };
					  {
 tree op_type = TREE_TYPE (captures[1]);
					      if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_COPYSIGN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2928;
						  {
						    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						    {
						      tree _o1[6], _r1;
						      _o1[0] = captures[0];
						      _o1[1] = captures[2];
						      _o1[2] = captures[3];
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[4];
							gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
							tem_op.resimplify (lseq, valueize);
							_r2 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r2) goto next_after_fail2928;
							_o1[3] = _r2;
						      }
						      _o1[4] = captures[5];
						      _o1[5] = captures[6];
						      gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_COPYSIGN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2928;
						      res_op->ops[0] = _r1;
						    }
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2928:;
						}
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
	      case PLUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_ADD), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2929;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2929;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_ADD, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2929;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2929:;
			  }
		    }
		  }
	          break;
	        }
	      case MINUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SUB), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2930;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2930;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SUB, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2930;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2930:;
			  }
		    }
		  }
	          break;
	        }
	      case MULT_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MUL), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2931;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2931;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MUL, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2931;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2931:;
			  }
		    }
		  }
	          break;
	        }
	      case TRUNC_DIV_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_DIV), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2932;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2932;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_DIV, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2932;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2932:;
			  }
		    }
		  }
	          break;
	        }
	      case TRUNC_MOD_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MOD), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2933;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2933;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MOD, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2933;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2933:;
			  }
		    }
		  }
	          break;
	        }
	      case RDIV_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_RDIV), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2934;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2934;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_RDIV, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2934;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2934:;
			  }
		    }
		  }
	          break;
	        }
	      case MIN_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MIN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2935;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2935;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MIN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2935;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2935:;
			  }
		    }
		  }
	          break;
	        }
	      case MAX_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MAX), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2936;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2936;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MAX, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2936;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2936:;
			  }
		    }
		  }
	          break;
	        }
	      case BIT_AND_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_AND), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2937;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2937;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_AND, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2937;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2937:;
			  }
		    }
		  }
	          break;
	        }
	      case BIT_IOR_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_IOR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2938;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2938;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_IOR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2938;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2938:;
			  }
		    }
		  }
	          break;
	        }
	      case BIT_XOR_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_XOR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2939;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2939;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_XOR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2939;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2939:;
			  }
		    }
		  }
	          break;
	        }
	      case LSHIFT_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SHL), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2940;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2940;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SHL, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2940;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2940:;
			  }
		    }
		  }
	          break;
	        }
	      case RSHIFT_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[1]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SHR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2941;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				_o1[0] = captures[0];
				_o1[1] = captures[2];
				_o1[2] = captures[3];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[4];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2941;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SHR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2941;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2941:;
			  }
		    }
		  }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_FMAX:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		      {
 tree op_type = TREE_TYPE (captures[1]);
			  if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_FMAX), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2942;
			      {
				res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				{
				  tree _o1[6], _r1;
				  _o1[0] = captures[0];
				  _o1[1] = captures[2];
				  _o1[2] = captures[3];
				  {
				    tree _o2[1], _r2;
				    _o2[0] = captures[4];
				    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail2942;
				    _o1[3] = _r2;
				  }
				  _o1[4] = captures[5];
				  _o1[5] = captures[6];
				  gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_FMAX, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail2942;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2942:;
			    }
		      }
		    }
	          }
	        break;
	      case CFN_FMIN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		      {
 tree op_type = TREE_TYPE (captures[1]);
			  if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_FMIN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2943;
			      {
				res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				{
				  tree _o1[6], _r1;
				  _o1[0] = captures[0];
				  _o1[1] = captures[2];
				  _o1[2] = captures[3];
				  {
				    tree _o2[1], _r2;
				    _o2[0] = captures[4];
				    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail2943;
				    _o1[3] = _r2;
				  }
				  _o1[4] = captures[5];
				  _o1[5] = captures[6];
				  gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_FMIN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail2943;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2943:;
			    }
		      }
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
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _p2, _p3, _p4 };
		      {
 tree op_type = TREE_TYPE (captures[1]);
			  if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_COPYSIGN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2944;
			      {
				res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				{
				  tree _o1[6], _r1;
				  _o1[0] = captures[0];
				  _o1[1] = captures[2];
				  _o1[2] = captures[3];
				  {
				    tree _o2[1], _r2;
				    _o2[0] = captures[4];
				    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail2944;
				    _o1[3] = _r2;
				  }
				  _o1[4] = captures[5];
				  _o1[5] = captures[6];
				  gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_COPYSIGN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail2944;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1239, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2944:;
			    }
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
  switch (TREE_CODE (_p2))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p2))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VIEW_CONVERT_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q40) == SSA_NAME
		       || is_gimple_min_invariant (_q40)))
		    {
		      _q40 = do_valueize (valueize, _q40);
		      switch (TREE_CODE (_q40))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q40))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  case PLUS_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (tree_swap_operands_p (_q50, _q51))
				        std::swap (_q50, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_ADD), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2945;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2945;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2945;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_ADD, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2945;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2945:;
					      }
					}
				      }
				      break;
				    }
				  case MINUS_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SUB), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2946;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2946;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2946;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SUB, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2946;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2946:;
					      }
					}
				      }
				      break;
				    }
				  case MULT_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (tree_swap_operands_p (_q50, _q51))
				        std::swap (_q50, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MUL), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2947;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2947;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2947;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MUL, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2947;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2947:;
					      }
					}
				      }
				      break;
				    }
				  case TRUNC_DIV_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_DIV), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2948;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2948;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2948;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_DIV, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2948;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2948:;
					      }
					}
				      }
				      break;
				    }
				  case TRUNC_MOD_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MOD), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2949;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2949;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2949;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MOD, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2949;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2949:;
					      }
					}
				      }
				      break;
				    }
				  case RDIV_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_RDIV), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2950;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2950;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2950;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_RDIV, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2950;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2950:;
					      }
					}
				      }
				      break;
				    }
				  case MIN_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (tree_swap_operands_p (_q50, _q51))
				        std::swap (_q50, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MIN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2951;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2951;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2951;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MIN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2951;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2951:;
					      }
					}
				      }
				      break;
				    }
				  case MAX_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (tree_swap_operands_p (_q50, _q51))
				        std::swap (_q50, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MAX), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2952;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2952;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2952;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MAX, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2952;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2952:;
					      }
					}
				      }
				      break;
				    }
				  case BIT_AND_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (tree_swap_operands_p (_q50, _q51))
				        std::swap (_q50, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_AND), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2953;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2953;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2953;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_AND, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2953;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2953:;
					      }
					}
				      }
				      break;
				    }
				  case BIT_IOR_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (tree_swap_operands_p (_q50, _q51))
				        std::swap (_q50, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_IOR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2954;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2954;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2954;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_IOR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2954;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2954:;
					      }
					}
				      }
				      break;
				    }
				  case BIT_XOR_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (tree_swap_operands_p (_q50, _q51))
				        std::swap (_q50, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_XOR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2955;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2955;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2955;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_XOR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2955;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2955:;
					      }
					}
				      }
				      break;
				    }
				  case LSHIFT_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SHL), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2956;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2956;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2956;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SHL, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2956;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2956:;
					      }
					}
				      }
				      break;
				    }
				  case RSHIFT_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      {
					tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					{
 tree op_type = TREE_TYPE (captures[2]);
					    if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SHR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2957;
						{
						  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						  {
						    tree _o1[6], _r1;
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[0];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2957;
						      _o1[0] = _r2;
						    }
						    _o1[1] = captures[3];
						    _o1[2] = captures[4];
						    {
						      tree _o2[1], _r2;
						      _o2[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2957;
						      _o1[3] = _r2;
						    }
						    _o1[4] = captures[5];
						    _o1[5] = captures[6];
						    gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SHR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2957;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2957:;
					      }
					}
				      }
				      break;
				    }
				  default:;
				  }
			      else if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			        switch (gimple_call_combined_fn (_c2))
				  {
				  case CFN_FMAX:
				    if (gimple_call_num_args (_c2) == 2)
				      {
					tree _q50 = gimple_call_arg (_c2, 0);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_call_arg (_c2, 1);
					_q51 = do_valueize (valueize, _q51);
					{
					  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					  {
 tree op_type = TREE_TYPE (captures[2]);
					      if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_FMAX), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2958;
						  {
						    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						    {
						      tree _o1[6], _r1;
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[0];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
							tem_op.resimplify (lseq, valueize);
							_r2 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r2) goto next_after_fail2958;
							_o1[0] = _r2;
						      }
						      _o1[1] = captures[3];
						      _o1[2] = captures[4];
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
							tem_op.resimplify (lseq, valueize);
							_r2 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r2) goto next_after_fail2958;
							_o1[3] = _r2;
						      }
						      _o1[4] = captures[5];
						      _o1[5] = captures[6];
						      gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_FMAX, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2958;
						      res_op->ops[0] = _r1;
						    }
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2958:;
						}
					  }
					}
				      }
				    break;
				  case CFN_FMIN:
				    if (gimple_call_num_args (_c2) == 2)
				      {
					tree _q50 = gimple_call_arg (_c2, 0);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_call_arg (_c2, 1);
					_q51 = do_valueize (valueize, _q51);
					{
					  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					  {
 tree op_type = TREE_TYPE (captures[2]);
					      if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_FMIN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2959;
						  {
						    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						    {
						      tree _o1[6], _r1;
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[0];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
							tem_op.resimplify (lseq, valueize);
							_r2 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r2) goto next_after_fail2959;
							_o1[0] = _r2;
						      }
						      _o1[1] = captures[3];
						      _o1[2] = captures[4];
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
							tem_op.resimplify (lseq, valueize);
							_r2 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r2) goto next_after_fail2959;
							_o1[3] = _r2;
						      }
						      _o1[4] = captures[5];
						      _o1[5] = captures[6];
						      gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_FMIN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2959;
						      res_op->ops[0] = _r1;
						    }
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2959:;
						}
					  }
					}
				      }
				    break;
				  case CFN_COPYSIGN:
				    if (gimple_call_num_args (_c2) == 2)
				      {
					tree _q50 = gimple_call_arg (_c2, 0);
					_q50 = do_valueize (valueize, _q50);
					tree _q51 = gimple_call_arg (_c2, 1);
					_q51 = do_valueize (valueize, _q51);
					{
					  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q50, _q51, _p3, _p4 };
					  {
 tree op_type = TREE_TYPE (captures[2]);
					      if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_COPYSIGN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2960;
						  {
						    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						    {
						      tree _o1[6], _r1;
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[0];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
							tem_op.resimplify (lseq, valueize);
							_r2 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r2) goto next_after_fail2960;
							_o1[0] = _r2;
						      }
						      _o1[1] = captures[3];
						      _o1[2] = captures[4];
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
							tem_op.resimplify (lseq, valueize);
							_r2 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r2) goto next_after_fail2960;
							_o1[3] = _r2;
						      }
						      _o1[4] = captures[5];
						      _o1[5] = captures[6];
						      gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_COPYSIGN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2960;
						      res_op->ops[0] = _r1;
						    }
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2960:;
						}
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
	      case PLUS_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  if (tree_swap_operands_p (_q40, _q41))
		    std::swap (_q40, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_ADD), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2961;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2961;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2961;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_ADD, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2961;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2961:;
			  }
		    }
		  }
	          break;
	        }
	      case MINUS_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SUB), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2962;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2962;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2962;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SUB, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2962;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2962:;
			  }
		    }
		  }
	          break;
	        }
	      case MULT_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  if (tree_swap_operands_p (_q40, _q41))
		    std::swap (_q40, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MUL), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2963;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2963;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2963;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MUL, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2963;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2963:;
			  }
		    }
		  }
	          break;
	        }
	      case TRUNC_DIV_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_DIV), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2964;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2964;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2964;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_DIV, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2964;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2964:;
			  }
		    }
		  }
	          break;
	        }
	      case TRUNC_MOD_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MOD), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2965;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2965;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2965;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MOD, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2965;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2965:;
			  }
		    }
		  }
	          break;
	        }
	      case RDIV_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_RDIV), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2966;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2966;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2966;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_RDIV, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2966;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2966:;
			  }
		    }
		  }
	          break;
	        }
	      case MIN_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  if (tree_swap_operands_p (_q40, _q41))
		    std::swap (_q40, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MIN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2967;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2967;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2967;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MIN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2967;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2967:;
			  }
		    }
		  }
	          break;
	        }
	      case MAX_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  if (tree_swap_operands_p (_q40, _q41))
		    std::swap (_q40, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_MAX), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2968;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2968;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2968;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_MAX, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2968;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2968:;
			  }
		    }
		  }
	          break;
	        }
	      case BIT_AND_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  if (tree_swap_operands_p (_q40, _q41))
		    std::swap (_q40, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_AND), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2969;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2969;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2969;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_AND, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2969;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2969:;
			  }
		    }
		  }
	          break;
	        }
	      case BIT_IOR_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  if (tree_swap_operands_p (_q40, _q41))
		    std::swap (_q40, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_IOR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2970;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2970;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2970;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_IOR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2970;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2970:;
			  }
		    }
		  }
	          break;
	        }
	      case BIT_XOR_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  if (tree_swap_operands_p (_q40, _q41))
		    std::swap (_q40, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_XOR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2971;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2971;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2971;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_XOR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2971;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2971:;
			  }
		    }
		  }
	          break;
	        }
	      case LSHIFT_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SHL), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2972;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2972;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2972;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SHL, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2972;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2972:;
			  }
		    }
		  }
	          break;
	        }
	      case RSHIFT_EXPR:
	        {
		  tree _q40 = gimple_assign_rhs1 (_a1);
		  _q40 = do_valueize (valueize, _q40);
		  tree _q41 = gimple_assign_rhs2 (_a1);
		  _q41 = do_valueize (valueize, _q41);
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		    {
 tree op_type = TREE_TYPE (captures[2]);
			if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_SHR), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2973;
			    {
			      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			      {
				tree _o1[6], _r1;
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[0];
				  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2973;
				  _o1[0] = _r2;
				}
				_o1[1] = captures[3];
				_o1[2] = captures[4];
				{
				  tree _o2[1], _r2;
				  _o2[0] = captures[1];
				  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r2) goto next_after_fail2973;
				  _o1[3] = _r2;
				}
				_o1[4] = captures[5];
				_o1[5] = captures[6];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_SHR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail2973;
				res_op->ops[0] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2973:;
			  }
		    }
		  }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_FMAX:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q40 = gimple_call_arg (_c1, 0);
		    _q40 = do_valueize (valueize, _q40);
		    tree _q41 = gimple_call_arg (_c1, 1);
		    _q41 = do_valueize (valueize, _q41);
		    {
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		      {
 tree op_type = TREE_TYPE (captures[2]);
			  if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_FMAX), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2974;
			      {
				res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				{
				  tree _o1[6], _r1;
				  {
				    tree _o2[1], _r2;
				    _o2[0] = captures[0];
				    gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail2974;
				    _o1[0] = _r2;
				  }
				  _o1[1] = captures[3];
				  _o1[2] = captures[4];
				  {
				    tree _o2[1], _r2;
				    _o2[0] = captures[1];
				    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail2974;
				    _o1[3] = _r2;
				  }
				  _o1[4] = captures[5];
				  _o1[5] = captures[6];
				  gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_FMAX, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail2974;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2974:;
			    }
		      }
		    }
	          }
	        break;
	      case CFN_FMIN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q40 = gimple_call_arg (_c1, 0);
		    _q40 = do_valueize (valueize, _q40);
		    tree _q41 = gimple_call_arg (_c1, 1);
		    _q41 = do_valueize (valueize, _q41);
		    {
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		      {
 tree op_type = TREE_TYPE (captures[2]);
			  if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_FMIN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2975;
			      {
				res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				{
				  tree _o1[6], _r1;
				  {
				    tree _o2[1], _r2;
				    _o2[0] = captures[0];
				    gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail2975;
				    _o1[0] = _r2;
				  }
				  _o1[1] = captures[3];
				  _o1[2] = captures[4];
				  {
				    tree _o2[1], _r2;
				    _o2[0] = captures[1];
				    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail2975;
				    _o1[3] = _r2;
				  }
				  _o1[4] = captures[5];
				  _o1[5] = captures[6];
				  gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_FMIN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail2975;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2975:;
			    }
		      }
		    }
	          }
	        break;
	      case CFN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q40 = gimple_call_arg (_c1, 0);
		    _q40 = do_valueize (valueize, _q40);
		    tree _q41 = gimple_call_arg (_c1, 1);
		    _q41 = do_valueize (valueize, _q41);
		    {
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _p2, _q40, _q41, _p3, _p4 };
		      {
 tree op_type = TREE_TYPE (captures[2]);
			  if (vectorized_internal_fn_supported_p (as_internal_fn (CFN_COND_LEN_COPYSIGN), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2976;
			      {
				res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				{
				  tree _o1[6], _r1;
				  {
				    tree _o2[1], _r2;
				    _o2[0] = captures[0];
				    gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail2976;
				    _o1[0] = _r2;
				  }
				  _o1[1] = captures[3];
				  _o1[2] = captures[4];
				  {
				    tree _o2[1], _r2;
				    _o2[0] = captures[1];
				    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail2976;
				    _o1[3] = _r2;
				  }
				  _o1[4] = captures[5];
				  _o1[5] = captures[6];
				  gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_LEN_COPYSIGN, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail2976;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1240, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2976:;
			    }
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VIEW_CONVERT_EXPR:
	        {
		  tree _q30 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q30) == SSA_NAME
		       || is_gimple_min_invariant (_q30)))
		    {
		      _q30 = do_valueize (valueize, _q30);
		      switch (TREE_CODE (_q30))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q30))
			    {
			      if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			        switch (gimple_call_combined_fn (_c2))
				  {
				  case CFN_FMA:
				    if (gimple_call_num_args (_c2) == 3)
				      {
					tree _q40 = gimple_call_arg (_c2, 0);
					_q40 = do_valueize (valueize, _q40);
					tree _q41 = gimple_call_arg (_c2, 1);
					_q41 = do_valueize (valueize, _q41);
					tree _q42 = gimple_call_arg (_c2, 2);
					_q42 = do_valueize (valueize, _q42);
					if (tree_swap_operands_p (_q40, _q41))
					  std::swap (_q40, _q41);
					{
					  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _q42, _p2, _p3, _p4 };
					  if (gimple_simplify_669 (res_op, seq, valueize, type, captures, CFN_FMA, CFN_COND_LEN_FMA))
					    return true;
					}
				      }
				    break;
				  case CFN_FMS:
				    if (gimple_call_num_args (_c2) == 3)
				      {
					tree _q40 = gimple_call_arg (_c2, 0);
					_q40 = do_valueize (valueize, _q40);
					tree _q41 = gimple_call_arg (_c2, 1);
					_q41 = do_valueize (valueize, _q41);
					tree _q42 = gimple_call_arg (_c2, 2);
					_q42 = do_valueize (valueize, _q42);
					if (tree_swap_operands_p (_q40, _q41))
					  std::swap (_q40, _q41);
					{
					  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _q42, _p2, _p3, _p4 };
					  if (gimple_simplify_669 (res_op, seq, valueize, type, captures, CFN_FMS, CFN_COND_LEN_FMS))
					    return true;
					}
				      }
				    break;
				  case CFN_FNMA:
				    if (gimple_call_num_args (_c2) == 3)
				      {
					tree _q40 = gimple_call_arg (_c2, 0);
					_q40 = do_valueize (valueize, _q40);
					tree _q41 = gimple_call_arg (_c2, 1);
					_q41 = do_valueize (valueize, _q41);
					tree _q42 = gimple_call_arg (_c2, 2);
					_q42 = do_valueize (valueize, _q42);
					if (tree_swap_operands_p (_q40, _q41))
					  std::swap (_q40, _q41);
					{
					  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _q42, _p2, _p3, _p4 };
					  if (gimple_simplify_669 (res_op, seq, valueize, type, captures, CFN_FNMA, CFN_COND_LEN_FNMA))
					    return true;
					}
				      }
				    break;
				  case CFN_FNMS:
				    if (gimple_call_num_args (_c2) == 3)
				      {
					tree _q40 = gimple_call_arg (_c2, 0);
					_q40 = do_valueize (valueize, _q40);
					tree _q41 = gimple_call_arg (_c2, 1);
					_q41 = do_valueize (valueize, _q41);
					tree _q42 = gimple_call_arg (_c2, 2);
					_q42 = do_valueize (valueize, _q42);
					if (tree_swap_operands_p (_q40, _q41))
					  std::swap (_q40, _q41);
					{
					  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _q42, _p2, _p3, _p4 };
					  if (gimple_simplify_669 (res_op, seq, valueize, type, captures, CFN_FNMS, CFN_COND_LEN_FNMS))
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
		    }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_FMA:
	        if (gimple_call_num_args (_c1) == 3)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    tree _q32 = gimple_call_arg (_c1, 2);
		    _q32 = do_valueize (valueize, _q32);
		    if (tree_swap_operands_p (_q30, _q31))
		      std::swap (_q30, _q31);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _p2, _p3, _p4 };
		      if (gimple_simplify_669 (res_op, seq, valueize, type, captures, CFN_FMA, CFN_COND_LEN_FMA))
		        return true;
		    }
	          }
	        break;
	      case CFN_FMS:
	        if (gimple_call_num_args (_c1) == 3)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    tree _q32 = gimple_call_arg (_c1, 2);
		    _q32 = do_valueize (valueize, _q32);
		    if (tree_swap_operands_p (_q30, _q31))
		      std::swap (_q30, _q31);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _p2, _p3, _p4 };
		      if (gimple_simplify_669 (res_op, seq, valueize, type, captures, CFN_FMS, CFN_COND_LEN_FMS))
		        return true;
		    }
	          }
	        break;
	      case CFN_FNMA:
	        if (gimple_call_num_args (_c1) == 3)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    tree _q32 = gimple_call_arg (_c1, 2);
		    _q32 = do_valueize (valueize, _q32);
		    if (tree_swap_operands_p (_q30, _q31))
		      std::swap (_q30, _q31);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _p2, _p3, _p4 };
		      if (gimple_simplify_669 (res_op, seq, valueize, type, captures, CFN_FNMA, CFN_COND_LEN_FNMA))
		        return true;
		    }
	          }
	        break;
	      case CFN_FNMS:
	        if (gimple_call_num_args (_c1) == 3)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    tree _q32 = gimple_call_arg (_c1, 2);
		    _q32 = do_valueize (valueize, _q32);
		    if (tree_swap_operands_p (_q30, _q31))
		      std::swap (_q30, _q31);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _p2, _p3, _p4 };
		      if (gimple_simplify_669 (res_op, seq, valueize, type, captures, CFN_FNMS, CFN_COND_LEN_FNMS))
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
  return false;
}
#pragma GCC diagnostic pop
