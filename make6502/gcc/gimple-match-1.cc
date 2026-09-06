/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "gimple-match-auto.h"

bool
gimple_logical_inverted_value (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
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
	      case TRUTH_NOT_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		    {
		      res_ops[0] = captures[0];
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 24, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case BIT_NOT_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  if (gimple_truth_valued_p (_p0, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			{
			  res_ops[0] = captures[0];
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 25, __FILE__, __LINE__, false);
			  return true;
			}
		      }
		    }
	          break;
	        }
	      case EQ_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			{
			  res_ops[0] = captures[0];
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 26, __FILE__, __LINE__, false);
			  return true;
			}
		      }
		    }
	          break;
	        }
	      case NE_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  if (gimple_truth_valued_p (_p0, valueize))
		    {
		      if (integer_truep (_p1))
			{
			  {
			    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			    {
			      res_ops[0] = captures[0];
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 27, __FILE__, __LINE__, false);
			      return true;
			    }
			  }
		        }
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
		  if (gimple_truth_valued_p (_p0, valueize))
		    {
		      if (integer_truep (_p1))
			{
			  {
			    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			    {
			      res_ops[0] = captures[0];
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 28, __FILE__, __LINE__, false);
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
      break;
    default:;
    }
  return false;
}

bool
gimple_unsigned_integer_sat_sub (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
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
			      case GT_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  switch (TREE_CODE (_p1))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _p1))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case MINUS_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
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
								      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 62, __FILE__, __LINE__, false);
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
			      case GE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
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
						  switch (TREE_CODE (_q50))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q50))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case MINUS_EXPR:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  tree _q61 = gimple_assign_rhs2 (_a4);
								  _q61 = do_valueize (valueize, _q61);
								  switch (TREE_CODE (_q60))
								    {
								    case SSA_NAME:
								      if (gimple *_d5 = get_def (valueize, _q60))
								        {
									  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									    switch (gimple_assign_rhs_code (_a5))
									      {
									      CASE_CONVERT:
									        {
										  tree _q70 = gimple_assign_rhs1 (_a5);
										  _q70 = do_valueize (valueize, _q70);
										  if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
										    {
										      switch (TREE_CODE (_q61))
										        {
											case SSA_NAME:
											  if (gimple *_d6 = get_def (valueize, _q61))
											    {
											      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
											        switch (gimple_assign_rhs_code (_a6))
												  {
												  CASE_CONVERT:
												    {
												      tree _q90 = gimple_assign_rhs1 (_a6);
												      _q90 = do_valueize (valueize, _q90);
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
														      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
									          break;
									        }
									      default:;
									      }
								        }
								      break;
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
										      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
					      case MINUS_EXPR:
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
							      CASE_CONVERT:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
								    {
								      switch (TREE_CODE (_q51))
								        {
									case SSA_NAME:
									  if (gimple *_d5 = get_def (valueize, _q51))
									    {
									      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									        switch (gimple_assign_rhs_code (_a5))
										  {
										  CASE_CONVERT:
										    {
										      tree _q80 = gimple_assign_rhs1 (_a5);
										      _q80 = do_valueize (valueize, _q80);
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
												      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
							          break;
							        }
							      default:;
							      }
						        }
						      break;
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
								      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
											case REALPART_EXPR:
											  {
											    tree _q80 = TREE_OPERAND (gimple_assign_rhs1 (_a5), 0);
											    if ((TREE_CODE (_q80) == SSA_NAME
											         || is_gimple_min_invariant (_q80)))
											      {
												_q80 = do_valueize (valueize, _q80);
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
														if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 64, __FILE__, __LINE__, false);
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
								  case CFN_SUB_OVERFLOW:
								    if (gimple_call_num_args (_c4) == 2)
								      {
									tree _q40 = gimple_call_arg (_c4, 0);
									_q40 = do_valueize (valueize, _q40);
									tree _q41 = gimple_call_arg (_c4, 1);
									_q41 = do_valueize (valueize, _q41);
									if (integer_zerop (_q21))
									  {
									    if (integer_zerop (_p1))
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
 && types_match (type, captures[0], captures[1])
)
													    {
													      {
														res_ops[0] = captures[0];
														res_ops[1] = captures[1];
														if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 65, __FILE__, __LINE__, false);
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
				  switch (TREE_CODE (_q21))
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
						    case MINUS_EXPR:
						      {
							tree _q50 = gimple_assign_rhs1 (_a3);
							_q50 = do_valueize (valueize, _q50);
							tree _q51 = gimple_assign_rhs2 (_a3);
							_q51 = do_valueize (valueize, _q51);
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
										      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 66, __FILE__, __LINE__, false);
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
								    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 62, __FILE__, __LINE__, false);
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
						tree _q50 = gimple_assign_rhs1 (_a2);
						_q50 = do_valueize (valueize, _q50);
						switch (TREE_CODE (_q50))
						  {
						  case SSA_NAME:
						    if (gimple *_d3 = get_def (valueize, _q50))
						      {
							if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							  switch (gimple_assign_rhs_code (_a3))
							    {
							    case MINUS_EXPR:
							      {
								tree _q60 = gimple_assign_rhs1 (_a3);
								_q60 = do_valueize (valueize, _q60);
								tree _q61 = gimple_assign_rhs2 (_a3);
								_q61 = do_valueize (valueize, _q61);
								switch (TREE_CODE (_q60))
								  {
								  case SSA_NAME:
								    if (gimple *_d4 = get_def (valueize, _q60))
								      {
									if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									  switch (gimple_assign_rhs_code (_a4))
									    {
									    CASE_CONVERT:
									      {
										tree _q70 = gimple_assign_rhs1 (_a4);
										_q70 = do_valueize (valueize, _q70);
										if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
										  {
										    switch (TREE_CODE (_q61))
										      {
										      case SSA_NAME:
										        if (gimple *_d5 = get_def (valueize, _q61))
										          {
											    if (gassign *_a5 = dyn_cast <gassign *> (_d5))
											      switch (gimple_assign_rhs_code (_a5))
												{
												CASE_CONVERT:
												  {
												    tree _q90 = gimple_assign_rhs1 (_a5);
												    _q90 = do_valueize (valueize, _q90);
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
														    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
									        break;
									      }
									    default:;
									    }
								      }
								    break;
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
										    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
					    case MINUS_EXPR:
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
							    CASE_CONVERT:
							      {
								tree _q60 = gimple_assign_rhs1 (_a3);
								_q60 = do_valueize (valueize, _q60);
								if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
								  {
								    switch (TREE_CODE (_q51))
								      {
								      case SSA_NAME:
								        if (gimple *_d4 = get_def (valueize, _q51))
								          {
									    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									      switch (gimple_assign_rhs_code (_a4))
										{
										CASE_CONVERT:
										  {
										    tree _q80 = gimple_assign_rhs1 (_a4);
										    _q80 = do_valueize (valueize, _q80);
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
												    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
							        break;
							      }
							    default:;
							    }
						      }
						    break;
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
								    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
										      case REALPART_EXPR:
										        {
											  tree _q80 = TREE_OPERAND (gimple_assign_rhs1 (_a4), 0);
											  if ((TREE_CODE (_q80) == SSA_NAME
											       || is_gimple_min_invariant (_q80)))
											    {
											      _q80 = do_valueize (valueize, _q80);
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
													      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 64, __FILE__, __LINE__, false);
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
								case CFN_SUB_OVERFLOW:
								  if (gimple_call_num_args (_c3) == 2)
								    {
								      tree _q40 = gimple_call_arg (_c3, 0);
								      _q40 = do_valueize (valueize, _q40);
								      tree _q41 = gimple_call_arg (_c3, 1);
								      _q41 = do_valueize (valueize, _q41);
								      if (integer_zerop (_q21))
									{
									  if (integer_zerop (_p1))
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
 && types_match (type, captures[0], captures[1])
)
													  {
													    {
													      res_ops[0] = captures[0];
													      res_ops[1] = captures[1];
													      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 65, __FILE__, __LINE__, false);
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
						  case MINUS_EXPR:
						    {
						      tree _q50 = gimple_assign_rhs1 (_a2);
						      _q50 = do_valueize (valueize, _q50);
						      tree _q51 = gimple_assign_rhs2 (_a2);
						      _q51 = do_valueize (valueize, _q51);
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
										    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 66, __FILE__, __LINE__, false);
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
					  break;
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
	          break;
	        }
	      case MULT_EXPR:
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
			      case MINUS_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
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
						  switch (TREE_CODE (_q50))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q50))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case GT_EXPR:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  tree _q61 = gimple_assign_rhs2 (_a4);
								  _q61 = do_valueize (valueize, _q61);
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
										  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 67, __FILE__, __LINE__, false);
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
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  tree _q61 = gimple_assign_rhs2 (_a4);
								  _q61 = do_valueize (valueize, _q61);
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
										  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 68, __FILE__, __LINE__, false);
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
			      CASE_CONVERT:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  switch (TREE_CODE (_q20))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q20))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case GT_EXPR:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  tree _q31 = gimple_assign_rhs2 (_a3);
						  _q31 = do_valueize (valueize, _q31);
						  switch (TREE_CODE (_p1))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _p1))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case MINUS_EXPR:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  tree _q61 = gimple_assign_rhs2 (_a4);
								  _q61 = do_valueize (valueize, _q61);
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
										  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 67, __FILE__, __LINE__, false);
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
					      case GE_EXPR:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  tree _q31 = gimple_assign_rhs2 (_a3);
						  _q31 = do_valueize (valueize, _q31);
						  switch (TREE_CODE (_p1))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _p1))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case MINUS_EXPR:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  tree _q61 = gimple_assign_rhs2 (_a4);
								  _q61 = do_valueize (valueize, _q61);
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
										  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 68, __FILE__, __LINE__, false);
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
			      case REALPART_EXPR:
			        {
				  tree _q20 = TREE_OPERAND (gimple_assign_rhs1 (_a2), 0);
				  if ((TREE_CODE (_q20) == SSA_NAME
				       || is_gimple_min_invariant (_q20)))
				    {
				      _q20 = do_valueize (valueize, _q20);
				      switch (TREE_CODE (_q20))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _q20))
					    {
					      if (gcall *_c3 = dyn_cast <gcall *> (_d3))
					        switch (gimple_call_combined_fn (_c3))
						  {
						  case CFN_SUB_OVERFLOW:
						    if (gimple_call_num_args (_c3) == 2)
						      {
							tree _q30 = gimple_call_arg (_c3, 0);
							_q30 = do_valueize (valueize, _q30);
							tree _q31 = gimple_call_arg (_c3, 1);
							_q31 = do_valueize (valueize, _q31);
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
									tree _q60 = gimple_assign_rhs1 (_a4);
									_q60 = do_valueize (valueize, _q60);
									tree _q61 = gimple_assign_rhs2 (_a4);
									_q61 = do_valueize (valueize, _q61);
									if (tree_swap_operands_p (_q60, _q61))
									  std::swap (_q60, _q61);
									switch (TREE_CODE (_q60))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q60))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case IMAGPART_EXPR:
										      {
											tree _q70 = TREE_OPERAND (gimple_assign_rhs1 (_a5), 0);
											if ((TREE_CODE (_q70) == SSA_NAME
											     || is_gimple_min_invariant (_q70)))
											  {
											    _q70 = do_valueize (valueize, _q70);
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
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 69, __FILE__, __LINE__, false);
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
			      case BIT_XOR_EXPR:
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
						      if (integer_onep (_q21))
							{
							  switch (TREE_CODE (_p1))
							    {
							    case SSA_NAME:
							      if (gimple *_d4 = get_def (valueize, _p1))
							        {
								  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								    switch (gimple_assign_rhs_code (_a4))
								      {
								      case REALPART_EXPR:
								        {
									  tree _q60 = TREE_OPERAND (gimple_assign_rhs1 (_a4), 0);
									  if ((TREE_CODE (_q60) == SSA_NAME
									       || is_gimple_min_invariant (_q60)))
									    {
									      _q60 = do_valueize (valueize, _q60);
									      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
										{
										  switch (TREE_CODE (_q60))
										    {
										    case SSA_NAME:
										      if (gimple *_d5 = get_def (valueize, _q60))
										        {
											  if (gcall *_c5 = dyn_cast <gcall *> (_d5))
											    switch (gimple_call_combined_fn (_c5))
											      {
											      case CFN_SUB_OVERFLOW:
											        if (gimple_call_num_args (_c5) == 2)
											          {
												    tree _q80 = gimple_call_arg (_c5, 0);
												    _q80 = do_valueize (valueize, _q80);
												    tree _q81 = gimple_call_arg (_c5, 1);
												    _q81 = do_valueize (valueize, _q81);
												    {
												      tree captures[3] ATTRIBUTE_UNUSED = { _q80, _q81, _q30 };
												      if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
													{
													  {
													    res_ops[0] = captures[0];
													    res_ops[1] = captures[1];
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 69, __FILE__, __LINE__, false);
													    return true;
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
	      case BIT_AND_EXPR:
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
			      case REALPART_EXPR:
			        {
				  tree _q20 = TREE_OPERAND (gimple_assign_rhs1 (_a2), 0);
				  if ((TREE_CODE (_q20) == SSA_NAME
				       || is_gimple_min_invariant (_q20)))
				    {
				      _q20 = do_valueize (valueize, _q20);
				      switch (TREE_CODE (_q20))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _q20))
					    {
					      if (gcall *_c3 = dyn_cast <gcall *> (_d3))
					        switch (gimple_call_combined_fn (_c3))
						  {
						  case CFN_SUB_OVERFLOW:
						    if (gimple_call_num_args (_c3) == 2)
						      {
							tree _q30 = gimple_call_arg (_c3, 0);
							_q30 = do_valueize (valueize, _q30);
							tree _q31 = gimple_call_arg (_c3, 1);
							_q31 = do_valueize (valueize, _q31);
							switch (TREE_CODE (_p1))
							  {
							  case SSA_NAME:
							    if (gimple *_d4 = get_def (valueize, _p1))
							      {
								if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								  switch (gimple_assign_rhs_code (_a4))
								    {
								    case PLUS_EXPR:
								      {
									tree _q60 = gimple_assign_rhs1 (_a4);
									_q60 = do_valueize (valueize, _q60);
									tree _q61 = gimple_assign_rhs2 (_a4);
									_q61 = do_valueize (valueize, _q61);
									if (tree_swap_operands_p (_q60, _q61))
									  std::swap (_q60, _q61);
									switch (TREE_CODE (_q60))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q60))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case IMAGPART_EXPR:
										      {
											tree _q70 = TREE_OPERAND (gimple_assign_rhs1 (_a5), 0);
											if ((TREE_CODE (_q70) == SSA_NAME
											     || is_gimple_min_invariant (_q70)))
											  {
											    _q70 = do_valueize (valueize, _q70);
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
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 70, __FILE__, __LINE__, false);
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
						      if (integer_minus_onep (_q21))
							{
							  switch (TREE_CODE (_p1))
							    {
							    case SSA_NAME:
							      if (gimple *_d4 = get_def (valueize, _p1))
							        {
								  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								    switch (gimple_assign_rhs_code (_a4))
								      {
								      case REALPART_EXPR:
								        {
									  tree _q60 = TREE_OPERAND (gimple_assign_rhs1 (_a4), 0);
									  if ((TREE_CODE (_q60) == SSA_NAME
									       || is_gimple_min_invariant (_q60)))
									    {
									      _q60 = do_valueize (valueize, _q60);
									      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
										{
										  switch (TREE_CODE (_q60))
										    {
										    case SSA_NAME:
										      if (gimple *_d5 = get_def (valueize, _q60))
										        {
											  if (gcall *_c5 = dyn_cast <gcall *> (_d5))
											    switch (gimple_call_combined_fn (_c5))
											      {
											      case CFN_SUB_OVERFLOW:
											        if (gimple_call_num_args (_c5) == 2)
											          {
												    tree _q80 = gimple_call_arg (_c5, 0);
												    _q80 = do_valueize (valueize, _q80);
												    tree _q81 = gimple_call_arg (_c5, 1);
												    _q81 = do_valueize (valueize, _q81);
												    {
												      tree captures[3] ATTRIBUTE_UNUSED = { _q80, _q81, _q30 };
												      if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
													{
													  {
													    res_ops[0] = captures[0];
													    res_ops[1] = captures[1];
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 70, __FILE__, __LINE__, false);
													    return true;
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
			      case MAX_EXPR:
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
							      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 71, __FILE__, __LINE__, false);
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
			      case GT_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  switch (TREE_CODE (_p1))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _p1))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case MINUS_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
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
								      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 62, __FILE__, __LINE__, false);
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
			      case GE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
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
						  switch (TREE_CODE (_q50))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q50))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case MINUS_EXPR:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  tree _q61 = gimple_assign_rhs2 (_a4);
								  _q61 = do_valueize (valueize, _q61);
								  switch (TREE_CODE (_q60))
								    {
								    case SSA_NAME:
								      if (gimple *_d5 = get_def (valueize, _q60))
								        {
									  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									    switch (gimple_assign_rhs_code (_a5))
									      {
									      CASE_CONVERT:
									        {
										  tree _q70 = gimple_assign_rhs1 (_a5);
										  _q70 = do_valueize (valueize, _q70);
										  if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
										    {
										      switch (TREE_CODE (_q61))
										        {
											case SSA_NAME:
											  if (gimple *_d6 = get_def (valueize, _q61))
											    {
											      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
											        switch (gimple_assign_rhs_code (_a6))
												  {
												  CASE_CONVERT:
												    {
												      tree _q90 = gimple_assign_rhs1 (_a6);
												      _q90 = do_valueize (valueize, _q90);
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
														      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
									          break;
									        }
									      default:;
									      }
								        }
								      break;
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
										      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
					      case MINUS_EXPR:
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
							      CASE_CONVERT:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
								    {
								      switch (TREE_CODE (_q51))
								        {
									case SSA_NAME:
									  if (gimple *_d5 = get_def (valueize, _q51))
									    {
									      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									        switch (gimple_assign_rhs_code (_a5))
										  {
										  CASE_CONVERT:
										    {
										      tree _q80 = gimple_assign_rhs1 (_a5);
										      _q80 = do_valueize (valueize, _q80);
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
												      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
							          break;
							        }
							      default:;
							      }
						        }
						      break;
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
								      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
											case REALPART_EXPR:
											  {
											    tree _q80 = TREE_OPERAND (gimple_assign_rhs1 (_a5), 0);
											    if ((TREE_CODE (_q80) == SSA_NAME
											         || is_gimple_min_invariant (_q80)))
											      {
												_q80 = do_valueize (valueize, _q80);
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
														if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 64, __FILE__, __LINE__, false);
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
								  case CFN_SUB_OVERFLOW:
								    if (gimple_call_num_args (_c4) == 2)
								      {
									tree _q40 = gimple_call_arg (_c4, 0);
									_q40 = do_valueize (valueize, _q40);
									tree _q41 = gimple_call_arg (_c4, 1);
									_q41 = do_valueize (valueize, _q41);
									if (integer_zerop (_q21))
									  {
									    if (integer_zerop (_p1))
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
 && types_match (type, captures[0], captures[1])
)
													    {
													      {
														res_ops[0] = captures[0];
														res_ops[1] = captures[1];
														if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 65, __FILE__, __LINE__, false);
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
				  switch (TREE_CODE (_q21))
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
						    case MINUS_EXPR:
						      {
							tree _q50 = gimple_assign_rhs1 (_a3);
							_q50 = do_valueize (valueize, _q50);
							tree _q51 = gimple_assign_rhs2 (_a3);
							_q51 = do_valueize (valueize, _q51);
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
										      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 66, __FILE__, __LINE__, false);
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
								    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 62, __FILE__, __LINE__, false);
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
						tree _q50 = gimple_assign_rhs1 (_a2);
						_q50 = do_valueize (valueize, _q50);
						switch (TREE_CODE (_q50))
						  {
						  case SSA_NAME:
						    if (gimple *_d3 = get_def (valueize, _q50))
						      {
							if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							  switch (gimple_assign_rhs_code (_a3))
							    {
							    case MINUS_EXPR:
							      {
								tree _q60 = gimple_assign_rhs1 (_a3);
								_q60 = do_valueize (valueize, _q60);
								tree _q61 = gimple_assign_rhs2 (_a3);
								_q61 = do_valueize (valueize, _q61);
								switch (TREE_CODE (_q60))
								  {
								  case SSA_NAME:
								    if (gimple *_d4 = get_def (valueize, _q60))
								      {
									if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									  switch (gimple_assign_rhs_code (_a4))
									    {
									    CASE_CONVERT:
									      {
										tree _q70 = gimple_assign_rhs1 (_a4);
										_q70 = do_valueize (valueize, _q70);
										if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
										  {
										    switch (TREE_CODE (_q61))
										      {
										      case SSA_NAME:
										        if (gimple *_d5 = get_def (valueize, _q61))
										          {
											    if (gassign *_a5 = dyn_cast <gassign *> (_d5))
											      switch (gimple_assign_rhs_code (_a5))
												{
												CASE_CONVERT:
												  {
												    tree _q90 = gimple_assign_rhs1 (_a5);
												    _q90 = do_valueize (valueize, _q90);
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
														    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
									        break;
									      }
									    default:;
									    }
								      }
								    break;
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
										    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
					    case MINUS_EXPR:
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
							    CASE_CONVERT:
							      {
								tree _q60 = gimple_assign_rhs1 (_a3);
								_q60 = do_valueize (valueize, _q60);
								if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
								  {
								    switch (TREE_CODE (_q51))
								      {
								      case SSA_NAME:
								        if (gimple *_d4 = get_def (valueize, _q51))
								          {
									    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									      switch (gimple_assign_rhs_code (_a4))
										{
										CASE_CONVERT:
										  {
										    tree _q80 = gimple_assign_rhs1 (_a4);
										    _q80 = do_valueize (valueize, _q80);
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
												    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
							        break;
							      }
							    default:;
							    }
						      }
						    break;
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
								    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
										      case REALPART_EXPR:
										        {
											  tree _q80 = TREE_OPERAND (gimple_assign_rhs1 (_a4), 0);
											  if ((TREE_CODE (_q80) == SSA_NAME
											       || is_gimple_min_invariant (_q80)))
											    {
											      _q80 = do_valueize (valueize, _q80);
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
													      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 64, __FILE__, __LINE__, false);
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
								case CFN_SUB_OVERFLOW:
								  if (gimple_call_num_args (_c3) == 2)
								    {
								      tree _q40 = gimple_call_arg (_c3, 0);
								      _q40 = do_valueize (valueize, _q40);
								      tree _q41 = gimple_call_arg (_c3, 1);
								      _q41 = do_valueize (valueize, _q41);
								      if (integer_zerop (_q21))
									{
									  if (integer_zerop (_p1))
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
 && types_match (type, captures[0], captures[1])
)
													  {
													    {
													      res_ops[0] = captures[0];
													      res_ops[1] = captures[1];
													      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 65, __FILE__, __LINE__, false);
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
						  case MINUS_EXPR:
						    {
						      tree _q50 = gimple_assign_rhs1 (_a2);
						      _q50 = do_valueize (valueize, _q50);
						      tree _q51 = gimple_assign_rhs2 (_a2);
						      _q51 = do_valueize (valueize, _q51);
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
										    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 66, __FILE__, __LINE__, false);
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
					  break;
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
    default:;
    }
  return false;
}

bool
gimple_simplify_249 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp),
 const enum tree_code ARG_UNUSED (ncmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (types_match (type, TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail921;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[1];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 367, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail921:;
    }
  return false;
}

bool
gimple_simplify_253 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (scmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (captures[2])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail926;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[2];
	res_op->ops[1] = captures[3];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 371, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail926:;
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail927;
	      {
		res_op->set_op (scmp, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] = captures[3];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 372, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail927:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail928;
	      {
		res_op->set_op (cmp, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] = captures[3];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 373, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail928:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_259 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (single_use (captures[1]) && single_use (captures[3])
)
    {
      {
 tree otype = TREE_TYPE (captures[0]);
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail937;
	  {
	    res_op->set_op (cmp, type, 2);
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[4];
	      if (otype != TREE_TYPE (_o1[0])
	          && !useless_type_conversion_p (otype, TREE_TYPE (_o1[0])))
		{
		  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, otype, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail937;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[0] = _r1;
	    }
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      if (otype != TREE_TYPE (_o1[0])
	          && !useless_type_conversion_p (otype, TREE_TYPE (_o1[0])))
		{
		  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, otype, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail937;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 380, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail937:;
      }
    }
  return false;
}

bool
gimple_simplify_267 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!integer_zerop (captures[1])
)
    {
      if (wi::to_wide (captures[2]) == 0
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail984;
	  {
	    res_op->set_op (cmp, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] = captures[2];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 428, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail984:;
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
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail985;
		      {
			tree tem;
			tem =  constant_boolean_node (cmp == NE_EXPR, type);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 429, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail985:;
		    }
		  else
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail986;
		      {
			res_op->set_op (cmp, type, 2);
			res_op->ops[0] = captures[0];
			res_op->ops[1] =  wide_int_to_tree (TREE_TYPE (captures[0]), prod);
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 430, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail986:;
		    }
	      }
	    }
	}
    }
  return false;
}

bool
gimple_simplify_270 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail989;
	      {
		res_op->set_op (cmp, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] =  force_fit_type (TREE_TYPE (captures[1]),
 wide_int::from (wi::to_wide (captures[2]),
 MAX (TYPE_PRECISION (TREE_TYPE (captures[2])),
 TYPE_PRECISION (TREE_TYPE (captures[1]))),
 TYPE_SIGN (TREE_TYPE (captures[2]))),
 0, TREE_OVERFLOW (captures[2]));
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 433, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail989:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail990;
	      {
		res_op->set_op (cmp, type, 2);
		res_op->ops[0] = captures[1];
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[2];
		  if (TREE_TYPE (res_op->ops[0]) != TREE_TYPE (_o1[0])
		      && !useless_type_conversion_p (TREE_TYPE (res_op->ops[0]), TREE_TYPE (_o1[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (res_op->ops[0]), _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail990;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[1] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 434, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail990:;
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail991;
		  {
		    res_op->set_op (cmp, type, 2);
		    res_op->ops[0] = captures[1];
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[3];
		      if (TREE_TYPE (res_op->ops[0]) != TREE_TYPE (_o1[0])
		          && !useless_type_conversion_p (TREE_TYPE (res_op->ops[0]), TREE_TYPE (_o1[0])))
			{
			  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (res_op->ops[0]), _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail991;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[1] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 435, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail991:;
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
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail992;
				  {
				    tree tem;
				    tem =  constant_boolean_node (cmp == EQ_EXPR ? false : true, type);
				    res_op->set_value (tem);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 436, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail992:;
				}
			      else
				{
				  if (cmp == LT_EXPR || cmp == LE_EXPR
)
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail993;
				      {
					tree tem;
					tem =  constant_boolean_node (above ? true : false, type);
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 437, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail993:;
				    }
				  else
				    {
				      if (cmp == GT_EXPR || cmp == GE_EXPR
)
					{
					  gimple_seq *lseq = seq;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail994;
					  {
					    tree tem;
					    tem =  constant_boolean_node (above ? false : true, type);
					    res_op->set_value (tem);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 438, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail994:;
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail995;
		  {
		    res_op->set_op (cmp, type, 2);
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[1];
		      if (newtype != TREE_TYPE (_o1[0])
		          && !useless_type_conversion_p (newtype, TREE_TYPE (_o1[0])))
			{
			  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, newtype, _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail995;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[0] = _r1;
		    }
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[3];
		      if (newtype != TREE_TYPE (_o1[0])
		          && !useless_type_conversion_p (newtype, TREE_TYPE (_o1[0])))
			{
			  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, newtype, _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail995;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[1] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 439, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail995:;
		}
	  }
	}
    }
  return false;
}

bool
gimple_simplify_291 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (neeq))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (POINTER_TYPE_P (TREE_TYPE (captures[0]))
 && ptrs_compare_unequal (captures[0], captures[1])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1039;
      {
	tree tem;
	tem =  constant_boolean_node (neeq != EQ_EXPR, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 466, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1039:;
    }
  return false;
}

bool
gimple_simplify_295 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1047;
	      {
		tree tem;
		tem =  constant_boolean_node (cmp == NE_EXPR, type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 474, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1047:;
	    }
	  else
	    {
	      if (single_use (captures[0])
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1048;
		  {
		    res_op->set_op (cmp, type, 2);
		    res_op->ops[0] = captures[1];
		    res_op->ops[1] =  TREE_OVERFLOW (res)
 ? drop_tree_overflow (res) : res;
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 475, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1048:;
		}
	    }
      }
    }
  return false;
}

bool
gimple_simplify_298 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (captures[3]) && single_use (captures[0])
)
    {
      {
 tree type0 = TREE_TYPE (captures[1]);
 tree stype = signed_type_for (TREE_TYPE (captures[1]));
 if (integer_zerop (captures[2]))
 stype = NULL_TREE;
	  if (stype
)
	    {
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1060;
	      {
		res_op->set_op (cmp, type, 2);
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[1];
		  if (stype != TREE_TYPE (_o1[0])
		      && !useless_type_conversion_p (stype, TREE_TYPE (_o1[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, stype, _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1060;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] =  build_zero_cst (stype);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 487, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1060:;
	    }
      }
    }
  else
    {
      {
 bool ok = true;
 tree type0 = TREE_TYPE (captures[1]);
 if (wi::to_widest (captures[2]) == TYPE_PRECISION (type0) - 1)
 ok = false;
	  if (ok && wi::to_wide (captures[3]) == (TYPE_PRECISION (type0) - 1)
)
	    {
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1061;
	      {
		res_op->set_op (op, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] =  build_one_cst (type0);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 488, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1061:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_306 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (cmp != NE_EXPR
 || ! FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 || ! tree_expr_maybe_nan_p (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1074;
      {
	tree tem;
	tem =  constant_boolean_node (false, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 501, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1074:;
    }
  return false;
}

bool
gimple_simplify_310 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1079;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 506, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1079:;
  return false;
}

bool
gimple_simplify_313 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1082;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[1];
	      _o1[1] = captures[3];
	      gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1082;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 509, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1082:;
	}
      else
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1083;
	  {
	    res_op->set_op (RSHIFT_EXPR, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] = captures[3];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 510, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1083:;
	}
    }
  return false;
}

bool
gimple_simplify_325 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1099;
	  {
	    res_op->set_op (div, type, 2);
	    res_op->ops[0] = captures[1];
	    res_op->ops[1] = captures[2];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 524, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1099:;
	}
      else
	{
	  {
int_range_max vr0, vr1, vr2;
	      if (get_range_query (cfun)->range_of_expr (vr0, captures[0])
 && get_range_query (cfun)->range_of_expr (vr1, captures[1])
 && get_range_query (cfun)->range_of_expr (vr2, captures[2])
 && range_op_handler (MULT_EXPR).overflow_free_p (vr0, vr1)
 && range_op_handler (MULT_EXPR).overflow_free_p (vr0, vr2)
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1100;
		  {
		    res_op->set_op (div, type, 2);
		    res_op->ops[0] = captures[1];
		    res_op->ops[1] = captures[2];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 525, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1100:;
		}
	  }
	}
    }
  return false;
}

bool
gimple_simplify_332 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!integer_zerop (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1107;
      {
	tree tem;
	tem =  build_zero_cst (type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 532, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1107:;
    }
  return false;
}

bool
gimple_simplify_340 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[2])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1117;
	  {
	    res_op->set_op (COS, type, 1);
	    res_op->ops[0] = captures[1];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 542, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1117:;
	}
    }
  return false;
}

bool
gimple_simplify_345 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POW))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (!TREE_OVERFLOW (captures[2])
)
	{
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1122;
	  {
	    res_op->set_op (POW, type, 2);
	    res_op->ops[0] = captures[1];
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[2];
	      _o1[1] =  build_one_cst (type);
	      gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1122;
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 547, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1122:;
	}
    }
  return false;
}

bool
gimple_simplify_350 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1129;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 554, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1129:;
    }
  return false;
}

bool
gimple_simplify_355 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1134;
	  {
	    tree tem;
	    tem =  constant_boolean_node (cmp == GE_EXPR || cmp == LE_EXPR, type);
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 559, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1134:;
	}
      else
	{
	  if (TREE_CODE (captures[1]) == INTEGER_CST && wi::neg_p (wi::to_wide (captures[1]))
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1135;
	      {
		tree tem;
		tem =  constant_boolean_node (cmp == LT_EXPR, type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 560, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1135:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_364 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1176;
	  {
	    res_op->set_op (out, type, 2);
	    res_op->ops[0] = captures[3];
	    res_op->ops[1] =  wide_int_to_tree (TREE_TYPE (captures[3]),
 wi::max_value (prec, sign)
 - wi::to_wide (captures[4]));
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 564, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1176:;
      }
    }
  return false;
}

bool
gimple_simplify_370 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1182;
		  {
		    res_op->set_op (cmp, type, 2);
		    res_op->ops[0] = captures[0];
		    res_op->ops[1] =  tem;
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 590, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1182:;
		}
	      else
		{
		  if (real_less (TREE_REAL_CST_PTR (captures[1]), &dconst0)
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1183;
		      {
			res_op->set_op (neg_cmp, type, 2);
			res_op->ops[0] = captures[0];
			res_op->ops[1] =  tem;
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 591, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1183:;
		    }
		}
	    }
      }
    }
  return false;
}

bool
gimple_simplify_375 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (acmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree cst = uniform_integer_cst_p (captures[1]);
      if (tree_int_cst_sgn (cst) == -1
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1194;
	  {
	    res_op->set_op (acmp, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] =  build_uniform_cst (TREE_TYPE (captures[1]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 + 1));
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 602, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1194:;
	}
  }
  return false;
}

bool
gimple_simplify_376 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (eqcmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
)
    {
      {
 tree ty = TREE_TYPE (captures[0]);
 unsigned prec = TYPE_PRECISION (ty);
 wide_int mask = wi::to_wide (captures[2], prec);
 wide_int rhs = wi::to_wide (captures[3], prec);
 signop sgn = TYPE_SIGN (ty);
	  if ((mask & (mask + 1)) == 0 && wi::gt_p (rhs, 0, sgn)
 && (rhs & (rhs + 1)) == 0 && wi::ge_p (mask, rhs, sgn)
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1195;
	      {
		res_op->set_op (eqcmp, type, 2);
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[1];
		  _o1[1] =  wide_int_to_tree (ty, mask - rhs);
		  gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1195;
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] =  build_zero_cst (ty);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 603, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1195:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_381 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1200;
	  {
	    res_op->set_op (cmp2, type, 2);
	    res_op->ops[0] = captures[1];
	    res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[1]));
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 604, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1200:;
	}
      else
	{
	  if (tree_int_cst_sgn (captures[2]) < 0
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1201;
	      {
		tree tem;
		tem =  constant_boolean_node (cmp == GT_EXPR ? true : false, type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 605, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1201:;
	    }
	  else
	    {
	      if (wi::to_widest (captures[2]) >= prec
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1202;
		  {
		    tree tem;
		    tem =  constant_boolean_node (cmp == GT_EXPR ? false : true, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 606, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1202:;
		}
	      else
		{
		  if (wi::to_widest (captures[2]) == prec - 1
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1203;
		      {
			res_op->set_op (cmp3, type, 2);
			res_op->ops[0] = captures[1];
			res_op->ops[1] =  wide_int_to_tree (TREE_TYPE (captures[1]),
 wi::shifted_mask (prec - 1, 1,
 false, prec));
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 607, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1203:;
		    }
		  else
		    {
		      if (single_use (captures[0])
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1204;
			  {
			    res_op->set_op (bit_op, type, 2);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[1];
			      _o1[1] =  build_zero_cst (TREE_TYPE (captures[1]));
			      gimple_match_op tem_op (res_op->cond.any_else (), cmp2, type, _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1204;
			      res_op->ops[0] = _r1;
			    }
			    {
			      tree _o1[2], _r1;
			      {
				tree _o2[2], _r2;
				_o2[0] = captures[1];
				_o2[1] =  wide_int_to_tree (TREE_TYPE (captures[1]),
 wi::mask (tree_to_uhwi (captures[2]),
 false, prec));
				gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				tem_op.resimplify (lseq, valueize);
				_r2 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r2) goto next_after_fail1204;
				_o1[0] = _r2;
			      }
			      _o1[1] =  build_zero_cst (TREE_TYPE (captures[1]));
			      gimple_match_op tem_op (res_op->cond.any_else (), cmp3, type, _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1204;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 608, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1204:;
			}
		    }
		}
	    }
	}
  }
  return false;
}

bool
gimple_simplify_395 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (BITS_PER_UNIT == 8
 && CHAR_TYPE_SIZE == 8
 && tree_fits_uhwi_p (captures[2])
)
    {
      {
 unsigned HOST_WIDE_INT prec = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned HOST_WIDE_INT bits = tree_to_uhwi (captures[2]);
 tree st = TYPE_PRECISION (type) > prec ? TREE_TYPE (captures[0]) : type;
	  if (TYPE_PRECISION (type) > prec
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && TYPE_UNSIGNED (type)
 && bits < prec && bits + 8 >= prec
)
	    {
	      {
 tree nst = build_int_cst (integer_type_node, prec - 8);
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1222;
		  {
		    res_op->set_op (RSHIFT_EXPR, type, 2);
		    {
		      tree _o1[1], _r1;
		      {
			tree _o2[2], _r2;
			{
			  tree _o3[1], _r3;
			  _o3[0] = captures[1];
			  if (st != TREE_TYPE (_o3[0])
			      && !useless_type_conversion_p (st, TREE_TYPE (_o3[0])))
			    {
			      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, st, _o3[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r3) goto next_after_fail1222;
			    }
			  else
			    _r3 = _o3[0];
			  _o2[0] = _r3;
			}
			_o2[1] = nst;
			gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, st, _o2[0], _o2[1]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1222;
			_o1[0] = _r2;
		      }
		      if (type != TREE_TYPE (_o1[0])
		          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
			{
			  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail1222;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[0] = _r1;
		    }
		    res_op->ops[1] = captures[2];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 624, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1222:;
	      }
	    }
	  else
	    {
	      if (bits + 8 == prec
)
		{
		  if (TYPE_UNSIGNED (st)
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1223;
		      {
			res_op->set_op (NOP_EXPR, type, 1);
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[1];
			  if (unsigned_char_type_node != TREE_TYPE (_o1[0])
			      && !useless_type_conversion_p (unsigned_char_type_node, TREE_TYPE (_o1[0])))
			    {
			      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, unsigned_char_type_node, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1223;
			    }
			  else
			    _r1 = _o1[0];
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 625, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1223:;
		    }
		  else
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1224;
		      {
			res_op->set_op (NOP_EXPR, type, 1);
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[1];
			  if (signed_char_type_node != TREE_TYPE (_o1[0])
			      && !useless_type_conversion_p (signed_char_type_node, TREE_TYPE (_o1[0])))
			    {
			      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, signed_char_type_node, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1224;
			    }
			  else
			    _r1 = _o1[0];
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 626, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1224:;
		    }
		}
	      else
		{
		  if (bits < prec && bits + 8 > prec
)
		    {
		      {
 tree nst = build_int_cst (integer_type_node, bits & 7);
 tree bt = TYPE_UNSIGNED (st) ? unsigned_char_type_node
 : signed_char_type_node;
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1225;
			  {
			    res_op->set_op (NOP_EXPR, type, 1);
			    {
			      tree _o1[2], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[1];
				if (bt != TREE_TYPE (_o2[0])
				    && !useless_type_conversion_p (bt, TREE_TYPE (_o2[0])))
				  {
				    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, bt, _o2[0]);
				    tem_op.resimplify (lseq, valueize);
				    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
				    if (!_r2) goto next_after_fail1225;
				  }
				else
				  _r2 = _o2[0];
				_o1[0] = _r2;
			      }
			      _o1[1] = nst;
			      gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, bt, _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1225;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 627, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1225:;
		      }
		    }
		}
	    }
      }
    }
  return false;
}

bool
gimple_simplify_412 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns),
 const combined_fn ARG_UNUSED (pows))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 HOST_WIDE_INT n;
      if (real_isinteger (&TREE_REAL_CST (captures[2]), &n) && (n & 1) == 0
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1256;
	  {
	    res_op->set_op (pows, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] = captures[2];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 646, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1256:;
	}
  }
  return false;
}

bool
gimple_simplify_413 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (pows),
 const combined_fn ARG_UNUSED (exps),
 const combined_fn ARG_UNUSED (logs),
 const combined_fn ARG_UNUSED (exp2s),
 const combined_fn ARG_UNUSED (log2s))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[0]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[0]))
 && canonicalize_math_after_vectorization_p ()
)
	{
	  {
 const REAL_VALUE_TYPE *const value = TREE_REAL_CST_PTR (captures[0]);
 bool use_exp2 = false;
 if (targetm.libc_has_function (function_c99_misc, TREE_TYPE (captures[0]))
 && value->cl == rvc_normal)
 {
 REAL_VALUE_TYPE frac_rvt = *value;
 SET_REAL_EXP (&frac_rvt, 1);
 if (real_equal (&frac_rvt, &dconst1))
 use_exp2 = true;
 }
	      if (!use_exp2
)
		{
		  if (optimize_pow_to_exp (captures[0], captures[1])
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1257;
		      {
			res_op->set_op (exps, type, 1);
			{
			  tree _o1[2], _r1;
			  {
			    tree _o2[1], _r2;
			    _o2[0] = captures[0];
			    gimple_match_op tem_op (res_op->cond.any_else (), logs, TREE_TYPE (_o2[0]), _o2[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail1257;
			    _o1[0] = _r2;
			  }
			  _o1[1] = captures[1];
			  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail1257;
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 647, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1257:;
		    }
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1258;
		  {
		    res_op->set_op (exp2s, type, 1);
		    {
		      tree _o1[2], _r1;
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[0];
			gimple_match_op tem_op (res_op->cond.any_else (), log2s, TREE_TYPE (_o2[0]), _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1258;
			_o1[0] = _r2;
		      }
		      _o1[1] = captures[1];
		      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1258;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 648, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1258:;
		}
	  }
	}
    }
  return false;
}

bool
gimple_simplify_419 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (HYPOT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1264;
      {
	res_op->set_op (MULT_EXPR, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1264;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] =  build_real_truncate (type, dconst_sqrt2 ());
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 654, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1264:;
    }
  return false;
}

bool
gimple_simplify_424 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
)
    {
      if (!TYPE_OVERFLOW_SANITIZED (type)
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[0]))
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1270;
	  {
	    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	    res_op->ops[0] = captures[0];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 660, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1270:;
	}
    }
  return false;
}

bool
gimple_simplify_429 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1275;
  {
    res_op->set_op (BIT_AND_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 665, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1275:;
  return false;
}

bool
gimple_simplify_435 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree pmop[2];
 tree utype = fold_bit_and_mask (TREE_TYPE (captures[1]), captures[5], op, captures[1], ERROR_MARK,
 NULL_TREE, NULL_TREE, captures[2], bitop, captures[3],
 captures[4], pmop);
      if (utype
)
	{
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[2])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1281;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[2], _r2;
		{
		  tree _o3[1], _r3;
		  _o3[0] =  pmop[0];
		  if (utype != TREE_TYPE (_o3[0])
		      && !useless_type_conversion_p (utype, TREE_TYPE (_o3[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o3[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r3) goto next_after_fail1281;
		    }
		  else
		    _r3 = _o3[0];
		  _o2[0] = _r3;
		}
		{
		  tree _o3[1], _r3;
		  _o3[0] =  pmop[1];
		  if (utype != TREE_TYPE (_o3[0])
		      && !useless_type_conversion_p (utype, TREE_TYPE (_o3[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o3[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r3) goto next_after_fail1281;
		    }
		  else
		    _r3 = _o3[0];
		  _o2[1] = _r3;
		}
		gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1281;
		_o1[0] = _r2;
	      }
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[5];
		if (utype != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail1281;
		  }
		else
		  _r2 = _o2[0];
		_o1[1] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1281;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 670, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1281:;
	}
  }
  return false;
}

bool
gimple_simplify_453 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (eqne == EQ_EXPR
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1305;
      {
	tree tem;
	tem =  constant_boolean_node (false, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 690, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1305:;
    }
  else
    {
      if (eqne == NE_EXPR
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1306;
	  {
	    tree tem;
	    tem = captures[0];
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 691, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1306:;
	}
    }
  return false;
}

bool
gimple_simplify_457 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (eqne == EQ_EXPR
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1328;
      {
	tree tem;
	tem =  constant_boolean_node (false, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 688, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1328:;
    }
  else
    {
      if (eqne == NE_EXPR
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1329;
	  {
	    tree tem;
	    tem = captures[0];
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 689, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1329:;
	}
    }
  return false;
}

bool
gimple_simplify_459 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (eqne == EQ_EXPR
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1331;
      {
	tree tem;
	tem =  constant_boolean_node (false, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 690, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1331:;
    }
  else
    {
      if (eqne == NE_EXPR
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1332;
	  {
	    tree tem;
	    tem = captures[0];
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 691, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1332:;
	}
    }
  return false;
}

bool
gimple_simplify_461 (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1334;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 692, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1334:;
	    }
	  else
	    {
	      if (code1 == EQ_EXPR && !val
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1335;
		  {
		    tree tem;
		    tem =  constant_boolean_node (false, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 693, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1335:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1336;
		      {
			tree tem;
			tem = captures[3];
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 694, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1336:;
		    }
		  else
		    {
		      if (code1 == NE_EXPR
 && code2 == GE_EXPR
 && cmp == 0
 && allbits
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1337;
			  {
			    res_op->set_op (GT_EXPR, type, 2);
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
				  if (!_r1) goto next_after_fail1337;
			        }
			      else
			        _r1 = _o1[0];
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 695, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1337:;
			}
		      else
			{
			  if (code1 == NE_EXPR
 && code2 == LE_EXPR
 && cmp == 0
 && allbits
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1338;
			      {
				res_op->set_op (LT_EXPR, type, 2);
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
				      if (!_r1) goto next_after_fail1338;
				    }
				  else
				    _r1 = _o1[0];
				  res_op->ops[1] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 696, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1338:;
			    }
			  else
			    {
			      if (code1 == NE_EXPR
 && code2 == GT_EXPR
 && one_after
 && allbits
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1339;
				  {
				    res_op->set_op (GT_EXPR, type, 2);
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
					  if (!_r1) goto next_after_fail1339;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 697, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1339:;
				}
			      else
				{
				  if (code1 == NE_EXPR
 && code2 == LT_EXPR
 && one_before
 && allbits
)
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1340;
				      {
					res_op->set_op (LT_EXPR, type, 2);
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
					      if (!_r1) goto next_after_fail1340;
					    }
					  else
					    _r1 = _o1[0];
					  res_op->ops[1] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 698, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1340:;
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
gimple_simplify_485 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1377;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] = captures[1];
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1377;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 725, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1377:;
    }
  return false;
}

bool
gimple_simplify_490 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1382;
	      {
		res_op->set_op (icmp, type, 2);
		res_op->ops[0] = captures[0];
		res_op->ops[1] = captures[1];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 728, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1382:;
	    }
	  else
	    {
	      if (ic == ncmp
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1383;
		  {
		    res_op->set_op (ncmp, type, 2);
		    res_op->ops[0] = captures[0];
		    res_op->ops[1] = captures[1];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 729, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1383:;
		}
	    }
      }
    }
  return false;
}

bool
gimple_simplify_502 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1407;
  {
    tree tem;
    tem = captures[1];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 740, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1407:;
  return false;
}

bool
gimple_simplify_504 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_signaling_nan_p (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1409;
      {
	tree tem;
	tem = captures[0];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 742, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1409:;
    }
  return false;
}

bool
gimple_simplify_508 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_UNDEFINED (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1413;
      {
	res_op->set_op (MAX_EXPR, type, 2);
	res_op->ops[0] = captures[2];
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 746, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1413:;
    }
  return false;
}

bool
gimple_simplify_516 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[0], captures[2])
 && tree_expr_nonzero_p (captures[3])
 && expr_no_side_effects_p (captures[2])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1421;
      {
	tree tem;
	tem = captures[1];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 754, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1421:;
    }
  return false;
}

bool
gimple_simplify_522 (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1441;
	      {
		res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
		res_op->ops[0] = captures[2];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 762, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1441:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1442;
	      {
		res_op->set_op (NOP_EXPR, type, 1);
		res_op->ops[0] = captures[2];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 763, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1442:;
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1443;
		  {
		    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
		    res_op->ops[0] = captures[0];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 764, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1443:;
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1444;
		  {
		    res_op->set_op (NOP_EXPR, type, 1);
		    res_op->ops[0] = captures[0];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 765, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1444:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1445;
			  {
			    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[2];
			      gimple_match_op tem_op (res_op->cond.any_else (), MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1445;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 766, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1445:;
			}
		      else
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1446;
			  {
			    res_op->set_op (NOP_EXPR, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[2];
			      gimple_match_op tem_op (res_op->cond.any_else (), MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1446;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 767, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1446:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1447;
			      {
				res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[0];
				  _o1[1] = captures[2];
				  gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail1447;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 768, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1447:;
			    }
			  else
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1448;
			      {
				res_op->set_op (NOP_EXPR, type, 1);
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[0];
				  _o1[1] = captures[2];
				  gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail1448;
				  res_op->ops[0] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 769, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1448:;
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1449;
				  {
				    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
				    res_op->ops[0] = captures[2];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 770, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1449:;
				}
			      else
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1450;
				  {
				    res_op->set_op (NOP_EXPR, type, 1);
				    res_op->ops[0] = captures[2];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 771, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1450:;
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
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1451;
				      {
					res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
					res_op->ops[0] = captures[0];
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 772, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1451:;
				    }
				  else
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1452;
				      {
					res_op->set_op (NOP_EXPR, type, 1);
					res_op->ops[0] = captures[0];
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 773, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1452:;
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
gimple_simplify_548 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1505;
      {
	tree tem;
	tem = captures[3];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 794, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1505:;
    }
  return false;
}

bool
gimple_simplify_555 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1512;
      {
	tree tem;
	tem = captures[0];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 798, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1512:;
    }
  return false;
}

bool
gimple_simplify_564 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == TYPE_PRECISION (type)
)
    {
      {
 tree shifter = build_int_cst (integer_type_node, TYPE_PRECISION (type) - 1);
	  if (cmp == LT_EXPR
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1526;
	      {
		res_op->set_op (BIT_XOR_EXPR, type, 2);
		{
		  tree _o1[1], _r1;
		  {
		    tree _o2[2], _r2;
		    _o2[0] = captures[0];
		    _o2[1] = shifter;
		    gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail1526;
		    _o1[0] = _r2;
		  }
		  if (type != TREE_TYPE (_o1[0])
		      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1526;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] = captures[1];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 806, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1526:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1527;
	      {
		res_op->set_op (BIT_NOT_EXPR, type, 1);
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    {
		      tree _o3[2], _r3;
		      _o3[0] = captures[0];
		      _o3[1] = shifter;
		      gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		      tem_op.resimplify (lseq, valueize);
		      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r3) goto next_after_fail1527;
		      _o2[0] = _r3;
		    }
		    if (type != TREE_TYPE (_o2[0])
		        && !useless_type_conversion_p (type, TREE_TYPE (_o2[0])))
		      {
			gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1527;
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  _o1[1] = captures[1];
		  gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1527;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 807, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1527:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_575 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_pow2p (captures[1])
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      {
 int shift = element_precision (captures[0]) - wi::exact_log2 (wi::to_wide (captures[1])) - 1;
	  if (shift >= 0
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1553;
	      {
		res_op->set_op (BIT_AND_EXPR, type, 2);
		{
		  tree _o1[1], _r1;
		  {
		    tree _o2[2], _r2;
		    _o2[0] = captures[0];
		    _o2[1] =  build_int_cst (integer_type_node, shift);
		    gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail1553;
		    _o1[0] = _r2;
		  }
		  if (type != TREE_TYPE (_o1[0])
		      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1553;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] = captures[1];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 815, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1553:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1554;
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
		      if (!_r1) goto next_after_fail1554;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] = captures[1];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 816, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1554:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_584 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (func))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 int val;
 internal_fn ifn = IFN_LAST;
 if (TREE_CODE (TREE_TYPE (captures[2])) == BITINT_TYPE)
 {
 if (tree_fits_shwi_p (captures[3]))
 {
 HOST_WIDE_INT valw = tree_to_shwi (captures[3]);
 if ((int) valw == valw)
 {
 val = valw;
 ifn = IFN_CTZ;
 }
 }
 }
 else if (direct_internal_fn_supported_p (IFN_CTZ, TREE_TYPE (captures[2]),
 OPTIMIZE_FOR_BOTH)
 && CTZ_DEFINED_VALUE_AT_ZERO
 (SCALAR_INT_TYPE_MODE (TREE_TYPE (captures[2])), val) == 2)
 ifn = IFN_CTZ;
      if (ifn == IFN_CTZ && wi::to_widest (captures[3]) == val
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1564;
	  {
	    res_op->set_op (CFN_CTZ, type, 2);
	    res_op->ops[0] = captures[2];
	    res_op->ops[1] = captures[3];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 823, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1564:;
	}
  }
  return false;
}

bool
gimple_simplify_592 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1574;
  {
    res_op->set_op (BIT_NOT_EXPR, type, 1);
    res_op->ops[0] = captures[0];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 831, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1574:;
  return false;
}

bool
gimple_simplify_599 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1581;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 782, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1581:;
  return false;
}

bool
gimple_simplify_605 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[5]);
      if (inverse_conditions_p (captures[0], captures[2])
 && element_precision (type) == element_precision (op_type)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1587;
	  {
	    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	    {
	      tree _o1[4], _r1;
	      _o1[0] = captures[2];
	      _o1[1] = captures[3];
	      _o1[2] = captures[4];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1587;
		_o1[3] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), cond_op, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1587;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 839, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1587:;
	}
  }
  return false;
}

bool
gimple_simplify_611 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_len_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[6]);
      if (inverse_conditions_p (captures[0], captures[2])
 && element_precision (type) == element_precision (op_type)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1593;
	  {
	    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	    {
	      tree _o1[7], _r1;
	      _o1[0] = captures[2];
	      _o1[1] = captures[3];
	      _o1[2] = captures[4];
	      _o1[3] = captures[5];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1593;
		_o1[4] = _r2;
	      }
	      _o1[5] = captures[7];
	      _o1[6] = captures[8];
	      gimple_match_op tem_op (res_op->cond.any_else (), cond_len_op, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5], _o1[6]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1593;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 845, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1593:;
	}
  }
  return false;
}

bool
gimple_simplify_623 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (sins),
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
      if (SCALAR_FLOAT_TYPE_P (type)
)
	{
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1605;
	  {
	    res_op->set_op (COND_EXPR, type, 3);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1605;
		_o1[0] = _r2;
	      }
	      _o1[1] =  t_cst;
	      gimple_match_op tem_op (res_op->cond.any_else (), LT_EXPR, boolean_type_node, _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1605;
	      res_op->ops[0] = _r1;
	    }
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[1];
	      {
		tree _o2[1], _r2;
		{
		  tree _o3[2], _r3;
		  {
		    tree _o4[2], _r4;
		    _o4[0] = captures[1];
		    _o4[1] = captures[1];
		    gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o4[0]), _o4[0], _o4[1]);
		    tem_op.resimplify (lseq, valueize);
		    _r4 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r4) goto next_after_fail1605;
		    _o3[0] = _r4;
		  }
		  _o3[1] =  t_one;
		  gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r3) goto next_after_fail1605;
		  _o2[0] = _r3;
		}
		gimple_match_op tem_op (res_op->cond.any_else (), sqrts, TREE_TYPE (_o2[0]), _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1605;
		_o1[1] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1605;
	      res_op->ops[1] = _r1;
	    }
	    {
	      tree _o1[2], _r1;
	      _o1[0] =  t_one;
	      _o1[1] = captures[1];
	      gimple_match_op tem_op (res_op->cond.any_else (), copysigns, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1605;
	      res_op->ops[2] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 857, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1605:;
	}
  }
  return false;
}

bool
gimple_simplify_647 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1629;
      {
	res_op->set_op (CFN_FMS, type, 3);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[1];
	res_op->ops[2] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 881, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1629:;
    }
  return false;
}

bool
gimple_simplify_655 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[1])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1639;
	      {
		res_op->set_op (popcount, type, 1);
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
			if (!_r2) goto next_after_fail1639;
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
		      if (!_r1) goto next_after_fail1639;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 888, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1639:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_664 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (PARITY))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) > TYPE_PRECISION (TREE_TYPE (captures[1]))
 && (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 || ((TYPE_PRECISION (TREE_TYPE (captures[0]))
 - TYPE_PRECISION (TREE_TYPE (captures[1]))) & 1) == 0)
)
    {
      {
 combined_fn cfn = CFN_LAST;
 tree type0 = TREE_TYPE (captures[1]);
 if (TREE_CODE (type0) == BITINT_TYPE)
 {
 if (TYPE_PRECISION (type0) > MAX_FIXED_MODE_SIZE)
 cfn = CFN_PARITY;
 else
 type0
 = build_nonstandard_integer_type (TYPE_PRECISION (type0),
 1);
 }
 type0 = unsigned_type_for (type0);
 if (cfn == CFN_LAST
 && direct_internal_fn_supported_p (IFN_PARITY, type0,
 OPTIMIZE_FOR_BOTH))
 cfn = CFN_PARITY;
 if (cfn == CFN_LAST
 && TYPE_PRECISION (TREE_TYPE (captures[0])) > BITS_PER_WORD
 && !direct_internal_fn_supported_p (IFN_PARITY,
 TREE_TYPE (captures[0]),
 OPTIMIZE_FOR_BOTH))
 {
 if (TYPE_PRECISION (type0)
 == TYPE_PRECISION (unsigned_type_node))
 cfn = CFN_BUILT_IN_PARITY;
 else if (TYPE_PRECISION (type0)
 == TYPE_PRECISION (long_long_unsigned_type_node))
 cfn = CFN_BUILT_IN_PARITYLL;
 }
	  if (cfn == CFN_PARITY
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1651;
	      {
		res_op->set_op (CFN_PARITY, type, 1);
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[1];
		  if (type0 != TREE_TYPE (_o1[0])
		      && !useless_type_conversion_p (type0, TREE_TYPE (_o1[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type0, _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1651;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 898, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1651:;
	    }
	  else
	    {
	      if (cfn == CFN_BUILT_IN_PARITY
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1652;
		  {
		    res_op->set_op (CFN_BUILT_IN_PARITY, type, 1);
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[1];
		      if (type0 != TREE_TYPE (_o1[0])
		          && !useless_type_conversion_p (type0, TREE_TYPE (_o1[0])))
			{
			  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type0, _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail1652;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 899, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1652:;
		}
	      else
		{
		  if (cfn == CFN_BUILT_IN_PARITYLL
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1653;
		      {
			res_op->set_op (CFN_BUILT_IN_PARITYLL, type, 1);
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[1];
			  if (type0 != TREE_TYPE (_o1[0])
			      && !useless_type_conversion_p (type0, TREE_TYPE (_o1[0])))
			    {
			      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type0, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1653;
			    }
			  else
			    _r1 = _o1[0];
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 900, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1653:;
		    }
		}
	    }
      }
    }
  return false;
}

bool
gimple_simplify_CFN_COS (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn coss = CFN_COS;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1700;
		    {
		      res_op->set_op (coss, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 932, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1700:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    const combined_fn coss = CFN_COS;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1701;
		    {
		      res_op->set_op (coss, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 932, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1701:;
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_COS))
		        return true;
		    }
	          }
	        break;
	      case CFN_ATAN:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		      if (gimple_simplify_410 (res_op, seq, valueize, type, captures, CFN_COS, CFN_ATAN, CFN_SQRT, CFN_COPYSIGN))
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_COS))
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_COS))
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_COS))
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
gimple_simplify_CFN_BUILT_IN_CCOSF (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1723;
		    {
		      res_op->set_op (CFN_BUILT_IN_CCOSF, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 944, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1723:;
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
gimple_simplify_CFN_BUILT_IN_CCOSL (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1725;
		    {
		      res_op->set_op (CFN_BUILT_IN_CCOSL, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 944, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1725:;
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
gimple_simplify_CFN_BUILT_IN_CCOSH (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1727;
		    {
		      res_op->set_op (CFN_BUILT_IN_CCOSH, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 944, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1727:;
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
gimple_simplify_CFN_BUILT_IN_CABS (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CONJ_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1734;
		    {
		      res_op->set_op (CFN_BUILT_IN_CABS, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 945, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1734:;
		  }
	          break;
	        }
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1735;
		    {
		      res_op->set_op (CFN_BUILT_IN_CABS, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 945, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1735:;
		  }
	          break;
	        }
	      case COMPLEX_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (real_zerop (_q21))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1736;
			{
			  res_op->set_op (ABS_EXPR, type, 1);
			  res_op->ops[0] = captures[0];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 946, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail1736:;
		      }
		    }
		  if (real_zerop (_q20))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1737;
			{
			  res_op->set_op (ABS_EXPR, type, 1);
			  res_op->ops[0] = captures[0];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 946, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail1737:;
		      }
		    }
		  if ((_q21 == _q20 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q20, 0) && types_match (_q21, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1738;
			    {
			      res_op->set_op (MULT_EXPR, type, 2);
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[0];
				gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail1738;
				res_op->ops[0] = _r1;
			      }
			      res_op->ops[1] =  build_real_truncate (type, dconst_sqrt2 ());
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 947, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail1738:;
			  }
		      }
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
gimple_simplify_FIX_TRUNC_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1779;
	{
	  tree tem;
	  tem = captures[0];
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 921, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail1779:;
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
1
 && useless_type_conversion_p (type, inside_type))
 || (
0
 && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (inside_type)))
 && (((inter_int || inter_ptr) && final_int)
 || (inter_float && final_float))
 && inter_prec >= final_prec
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1780;
			    {
			      res_op->set_op (FIX_TRUNC_EXPR, type, 1);
			      res_op->ops[0] = captures[1];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 922, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail1780:;
			  }
			else
			  {
			    if (((inter_int && inside_int) || (inter_float && inside_float))
 && (final_int || final_float)
 && inter_prec >= inside_prec
 && (inter_float || inter_unsignedp == inside_unsignedp)
)
			      {
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1781;
				{
				  res_op->set_op (FIX_TRUNC_EXPR, type, 1);
				  res_op->ops[0] = captures[1];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 923, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail1781:;
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
				    gimple_seq *lseq = seq;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1782;
				    {
				      res_op->set_op (FIX_TRUNC_EXPR, type, 1);
				      res_op->ops[0] = captures[1];
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 924, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail1782:;
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
					gimple_seq *lseq = seq;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1783;
					{
					  res_op->set_op (FIX_TRUNC_EXPR, type, 1);
					  res_op->ops[0] = captures[1];
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 925, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail1783:;
				      }
				    else
				      {
					if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1784;
					    {
					      res_op->set_op (NOP_EXPR, type, 1);
					      res_op->ops[0] = captures[1];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 926, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail1784:;
					  }
					else
					  {
					    if (1
 && final_int && inter_int && inside_int
 && final_prec == inside_prec
 && final_prec > inter_prec
 && inter_unsignedp
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1785;
						{
						  res_op->set_op (NOP_EXPR, type, 1);
						  {
						    tree _o1[2], _r1;
						    _o1[0] = captures[1];
						    _o1[1] =  wide_int_to_tree
 (inside_type,
 wi::mask (inter_prec, false,
 TYPE_PRECISION (inside_type)));
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail1785;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 927, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail1785:;
					      }
					    else
					      {
						if (1
 && inside_int && inter_float && final_int &&
 (unsigned) significand_size (TYPE_MODE (inter_type))
 >= inside_prec - !inside_unsignedp
)
						  {
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1786;
						    {
						      res_op->set_op (NOP_EXPR, type, 1);
						      res_op->ops[0] = captures[1];
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 928, __FILE__, __LINE__, true);
						      return true;
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
	      case FLOAT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
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
1
 && useless_type_conversion_p (type, inside_type))
 || (
0
 && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (inside_type)))
 && (((inter_int || inter_ptr) && final_int)
 || (inter_float && final_float))
 && inter_prec >= final_prec
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1787;
			    {
			      res_op->set_op (FIX_TRUNC_EXPR, type, 1);
			      res_op->ops[0] = captures[1];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 922, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail1787:;
			  }
			else
			  {
			    if (((inter_int && inside_int) || (inter_float && inside_float))
 && (final_int || final_float)
 && inter_prec >= inside_prec
 && (inter_float || inter_unsignedp == inside_unsignedp)
)
			      {
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1788;
				{
				  res_op->set_op (FIX_TRUNC_EXPR, type, 1);
				  res_op->ops[0] = captures[1];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 923, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail1788:;
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
				    gimple_seq *lseq = seq;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1789;
				    {
				      res_op->set_op (FIX_TRUNC_EXPR, type, 1);
				      res_op->ops[0] = captures[1];
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 924, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail1789:;
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
					gimple_seq *lseq = seq;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1790;
					{
					  res_op->set_op (FIX_TRUNC_EXPR, type, 1);
					  res_op->ops[0] = captures[1];
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 925, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail1790:;
				      }
				    else
				      {
					if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1791;
					    {
					      res_op->set_op (NOP_EXPR, type, 1);
					      res_op->ops[0] = captures[1];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 926, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail1791:;
					  }
					else
					  {
					    if (1
 && final_int && inter_int && inside_int
 && final_prec == inside_prec
 && final_prec > inter_prec
 && inter_unsignedp
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1792;
						{
						  res_op->set_op (NOP_EXPR, type, 1);
						  {
						    tree _o1[2], _r1;
						    _o1[0] = captures[1];
						    _o1[1] =  wide_int_to_tree
 (inside_type,
 wi::mask (inter_prec, false,
 TYPE_PRECISION (inside_type)));
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail1792;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 927, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail1792:;
					      }
					    else
					      {
						if (1
 && inside_int && inter_float && final_int &&
 (unsigned) significand_size (TYPE_MODE (inter_type))
 >= inside_prec - !inside_unsignedp
)
						  {
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1793;
						    {
						      res_op->set_op (NOP_EXPR, type, 1);
						      res_op->ops[0] = captures[1];
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 928, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail1793:;
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_BSWAP32 (gimple_match_op *res_op, gimple_seq *seq,
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
			      case CFN_BUILT_IN_BSWAP32:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
				      if (gimple_simplify_592 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32))
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
			      case CFN_BUILT_IN_BSWAP32:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR, CFN_BUILT_IN_BSWAP32))
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
			      case CFN_BUILT_IN_BSWAP32:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q40 = gimple_call_arg (_c2, 0);
				    _q40 = do_valueize (valueize, _q40);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR, CFN_BUILT_IN_BSWAP32))
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
			      case CFN_BUILT_IN_BSWAP32:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, CFN_BUILT_IN_BSWAP32))
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
			      case CFN_BUILT_IN_BSWAP32:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q40 = gimple_call_arg (_c2, 0);
				    _q40 = do_valueize (valueize, _q40);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, CFN_BUILT_IN_BSWAP32))
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
			      case CFN_BUILT_IN_BSWAP32:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, CFN_BUILT_IN_BSWAP32))
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
			      case CFN_BUILT_IN_BSWAP32:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q40 = gimple_call_arg (_c2, 0);
				    _q40 = do_valueize (valueize, _q40);
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				      if (gimple_simplify_593 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, CFN_BUILT_IN_BSWAP32))
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
	      case CFN_BUILT_IN_BSWAP32:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_591 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32))
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
gimple_simplify_CFN_LOG (gimple_match_op *res_op, gimple_seq *seq,
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
			  if (gimple_simplify_620 (res_op, seq, valueize, type, captures, CFN_LOG))
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
		      if (gimple_simplify_619 (res_op, seq, valueize, type, captures, CFN_LOG))
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1842;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 977, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1842:;
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
		      if (gimple_simplify_621 (res_op, seq, valueize, type, captures, CFN_LOG, CFN_POW))
		        return true;
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
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_EXP2)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1843;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_LOG, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1843;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1843:;
			      }
			    }
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
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_SQRT)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1844;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_LOG, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1844;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1844:;
			      }
			    }
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
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_EXP10)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1845;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_LOG, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1845;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1845:;
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
gimple_simplify_CFN_LOG2 (gimple_match_op *res_op, gimple_seq *seq,
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
			  if (gimple_simplify_620 (res_op, seq, valueize, type, captures, CFN_LOG2))
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
		      if (gimple_simplify_619 (res_op, seq, valueize, type, captures, CFN_LOG2))
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
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_EXP)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1864;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_LOG2, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1864;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1864:;
			      }
			    }
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
		      if (gimple_simplify_621 (res_op, seq, valueize, type, captures, CFN_LOG2, CFN_POW))
		        return true;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1865;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 977, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1865:;
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
		      if (flag_unsafe_math_optimizations
)
			{
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_SQRT)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1866;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_LOG2, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1866;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1866:;
			      }
			    }
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
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_EXP10)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1867;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_LOG2, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1867;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1867:;
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
gimple_simplify_CFN_BUILT_IN_EXPF (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_LOGF:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1890;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1890:;
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
gimple_simplify_CFN_BUILT_IN_EXP (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_LOG:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1891;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1891:;
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
gimple_simplify_CFN_EXP (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_LOG:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1893;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1893:;
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
gimple_simplify_CFN_EXP2 (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_LOG2:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1897;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1897:;
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
gimple_simplify_CFN_BUILT_IN_POW10F (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_LOG10F:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1902;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1902:;
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
gimple_simplify_CFN_BUILT_IN_CBRTF (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_SQRTF:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1931;
			  {
			    res_op->set_op (CFN_BUILT_IN_POWF, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] =  build_real_truncate (type, dconst_sixth ());
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 983, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1931:;
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
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1932;
			  {
			    res_op->set_op (CFN_BUILT_IN_EXP10F, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real_truncate (type, dconst_third ());
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1932;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1932:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1933;
			  {
			    res_op->set_op (CFN_BUILT_IN_EXPF, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real_truncate (type, dconst_third ());
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1933;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1933:;
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
		    if (tree_expr_nonnegative_p (_q20))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1934;
			      {
				res_op->set_op (CFN_BUILT_IN_POWF, type, 2);
				res_op->ops[0] = captures[0];
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[1];
				  _o1[1] =  build_real_truncate (type, dconst_third ());
				  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail1934;
				  res_op->ops[1] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 985, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1934:;
			    }
			}
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
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1935;
			  {
			    res_op->set_op (CFN_BUILT_IN_POW10F, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real_truncate (type, dconst_third ());
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1935;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1935:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CBRTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    if (tree_expr_nonnegative_p (_q20))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1936;
			      {
				res_op->set_op (CFN_BUILT_IN_POWF, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] =  build_real_truncate (type, dconst_ninth ());
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 986, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1936:;
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
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1937;
			  {
			    res_op->set_op (CFN_BUILT_IN_EXP2F, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real_truncate (type, dconst_third ());
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1937;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1937:;
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
gimple_simplify_CFN_BUILT_IN_SINL (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_ATANL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		      if (gimple_simplify_623 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SINL, CFN_BUILT_IN_ATANL, CFN_BUILT_IN_SQRTL, CFN_BUILT_IN_COPYSIGNL))
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
gimple_simplify_CFN_BUILT_IN_TRUNC (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_627 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNC, CFN_BUILT_IN_TRUNCF))
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
	      case CFN_BUILT_IN_TRUNC:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNC))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNC))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FLOORF (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_FLOORF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF))
        return true;
    }
  }
if (tree_expr_nonnegative_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_629 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF, CFN_BUILT_IN_TRUNCF))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FLOORF32X (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_FLOORF32X:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF32X))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF32X))
        return true;
    }
  }
if (tree_expr_nonnegative_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_629 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF32X, CFN_BUILT_IN_TRUNCF32X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_CEILF16 (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_CEILF16:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILF16))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILF16))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_CEILF64X (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_CEILF64X:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILF64X))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILF64X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_ROUNDF32 (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_ROUNDF32:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDF32))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDF32))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_ROUNDF128X (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_ROUNDF128X:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDF128X))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDF128X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_NEARBYINTF64 (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_NEARBYINTF64:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINTF64))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINTF64))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_RINTF (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_RINTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTF))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTF))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_RINTF32X (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_RINTF32X:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTF32X))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTF32X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_IFLOORL (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_632 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IFLOORL, CFN_BUILT_IN_IFLOOR))
			  return true;
		      }
		    }
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IFLOORL, CFN_BUILT_IN_IFLOORF))
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
      if (gimple_simplify_634 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IFLOORL))
        return true;
    }
  }
if (integer_valued_real_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IFLOORL))
        return true;
    }
  }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_636 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IFLOORL, CFN_BUILT_IN_LFLOORL, CFN_BUILT_IN_LLFLOORL))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LCEILL (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_632 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LCEILL, CFN_BUILT_IN_LCEIL))
			  return true;
		      }
		    }
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LCEILL, CFN_BUILT_IN_LCEILF))
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
      if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LCEILL))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LLRINTL (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_632 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LLRINTL, CFN_BUILT_IN_LLRINT))
			  return true;
		      }
		    }
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LLRINTL, CFN_BUILT_IN_LLRINTF))
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
      if (gimple_simplify_638 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LLRINTL))
        return true;
    }
  }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_637 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IRINTL, CFN_BUILT_IN_LRINTL, CFN_BUILT_IN_LLRINTL))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_IROUND (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IROUND, CFN_BUILT_IN_IROUNDF))
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
      if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IROUND))
        return true;
    }
  }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_636 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IROUND, CFN_BUILT_IN_LROUND, CFN_BUILT_IN_LLROUND))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LFLOORF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_expr_nonnegative_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_634 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LFLOORF))
	  return true;
      }
    }
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LFLOORF))
	  return true;
      }
    }
  return false;
}

bool
gimple_simplify_CFN_LLFLOOR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_expr_nonnegative_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_634 (res_op, seq, valueize, type, captures, CFN_LLFLOOR))
	  return true;
      }
    }
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_LLFLOOR))
	  return true;
      }
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_637 (res_op, seq, valueize, type, captures, CFN_IFLOOR, CFN_LFLOOR, CFN_LLFLOOR))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LLROUNDF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LLROUNDF))
	  return true;
      }
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_637 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IROUNDF, CFN_BUILT_IN_LROUNDF, CFN_BUILT_IN_LLROUNDF))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_LLRINT (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_638 (res_op, seq, valueize, type, captures, CFN_LLRINT))
	  return true;
      }
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_637 (res_op, seq, valueize, type, captures, CFN_IRINT, CFN_LRINT, CFN_LLRINT))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_SIGNBIT (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_expr_nonnegative_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_639 (res_op, seq, valueize, type, captures, CFN_SIGNBIT))
	  return true;
      }
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_640 (res_op, seq, valueize, type, captures, CFN_SIGNBIT))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_CTZIMAX (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_652 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZIMAX))
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
		    if (gimple_simplify_653 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZIMAX))
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
		    if (gimple_simplify_654 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZIMAX))
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
			if (gimple_simplify_650 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZIMAX))
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
			if (gimple_simplify_651 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CTZIMAX))
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
gimple_simplify_CFN_BUILT_IN_PARITY (gimple_match_op *res_op, gimple_seq *seq,
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
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    if (gimple_simplify_661 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_PARITY))
		      return true;
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
			      case LROTATE_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
				    if (gimple_simplify_663 (res_op, seq, valueize, type, captures, LROTATE_EXPR, CFN_BUILT_IN_PARITY))
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
				    if (gimple_simplify_663 (res_op, seq, valueize, type, captures, RROTATE_EXPR, CFN_BUILT_IN_PARITY))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128, CFN_BUILT_IN_PARITY))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16, CFN_BUILT_IN_PARITY))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32, CFN_BUILT_IN_PARITY))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64, CFN_BUILT_IN_PARITY))
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
		    if (gimple_simplify_664 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_PARITY))
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
		    if (gimple_simplify_666 (res_op, seq, valueize, type, captures, LROTATE_EXPR, CFN_BUILT_IN_PARITY))
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
		    if (gimple_simplify_666 (res_op, seq, valueize, type, captures, RROTATE_EXPR, CFN_BUILT_IN_PARITY))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128, CFN_BUILT_IN_PARITY))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16, CFN_BUILT_IN_PARITY))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32, CFN_BUILT_IN_PARITY))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64, CFN_BUILT_IN_PARITY))
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
    if (gimple_simplify_660 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_PARITY))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FFSIMAX (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_668 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FFSIMAX))
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
      if (gimple_simplify_667 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FFSIMAX, CFN_BUILT_IN_CTZIMAX))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_REDUC_XOR (gimple_match_op *res_op, gimple_seq *seq,
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
	      case VIEW_CONVERT_EXPR:
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
					case VECTOR_CST:
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
					      if (gimple_simplify_671 (res_op, seq, valueize, type, captures, CFN_REDUC_XOR))
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
		        default:;
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
		  switch (TREE_CODE (_q21))
		    {
		    case VECTOR_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			  if (gimple_simplify_671 (res_op, seq, valueize, type, captures, CFN_REDUC_XOR))
			    return true;
			}
		        break;
		      }
		    default:;
		    }
	          break;
	        }
	      case CONSTRUCTOR:
	        {
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		    if (gimple_simplify_672 (res_op, seq, valueize, type, captures, CFN_REDUC_XOR))
		      return true;
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
		    case VECTOR_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1966;
			  {
			    res_op->set_op (BIT_XOR_EXPR, type, 2);
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[0];
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_XOR, type, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1966;
			      res_op->ops[0] = _r1;
			    }
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_XOR, type, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1966;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 991, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1966:;
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
gimple_simplify_POINTER_PLUS_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2054;
	{
	  tree tem;
	  tem = captures[0];
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 992, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail2054:;
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
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		    gimple_seq *lseq = seq;
		    if (lseq
		        && (!single_use (captures[0])))
		      lseq = NULL;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2055;
		    {
		      res_op->set_op (POINTER_PLUS_EXPR, type, 2);
		      res_op->ops[0] = captures[1];
		      {
			tree _o1[2], _r1;
			_o1[0] = captures[2];
			_o1[1] = captures[3];
			gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			tem_op.resimplify (lseq, valueize);
			_r1 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r1) goto next_after_fail2055;
			res_op->ops[1] = _r1;
		      }
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1008, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2055:;
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2056;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1009, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail2056:;
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
			      case MINUS_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
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
						  switch (TREE_CODE (_q41))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q41))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      CASE_CONVERT:
							        {
								  tree _q70 = gimple_assign_rhs1 (_a4);
								  _q70 = do_valueize (valueize, _q70);
								  if ((_q70 == _p0 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _p0, 0) && types_match (_q70, _p0)))
								    {
								      {
									tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q50 };
									if (TYPE_PRECISION (TREE_TYPE (captures[1])) == TYPE_PRECISION (TREE_TYPE (captures[2]))
 && ((
1
 && useless_type_conversion_p (type, TREE_TYPE (captures[3])))
 || (
0
 && type == TREE_TYPE (captures[3])))
)
									  {
									    gimple_seq *lseq = seq;
									    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2057;
									    {
									      tree tem;
									      tem = captures[3];
									      res_op->set_value (tem);
									      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
									      return true;
									    }
next_after_fail2057:;
									  }
								      }
								    }
							          break;
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
			      case POINTER_DIFF_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _p1, _q30, _q40, _p0 };
					if (TYPE_PRECISION (TREE_TYPE (captures[1])) >= TYPE_PRECISION (TREE_TYPE (captures[2]))
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2058;
					    {
					      res_op->set_op (NOP_EXPR, type, 1);
					      res_op->ops[0] = captures[3];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2058:;
					  }
				      }
				    }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
		    }
	          break;
	        }
	      case MINUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  switch (TREE_CODE (_q30))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q30))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  switch (TREE_CODE (_q31))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q31))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
						    {
						      {
							tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _p1, _q40 };
							if (TYPE_PRECISION (TREE_TYPE (captures[1])) == TYPE_PRECISION (TREE_TYPE (captures[2]))
 && ((
1
 && useless_type_conversion_p (type, TREE_TYPE (captures[3])))
 || (
0
 && type == TREE_TYPE (captures[3])))
)
							  {
							    gimple_seq *lseq = seq;
							    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2059;
							    {
							      tree tem;
							      tem = captures[3];
							      res_op->set_value (tem);
							      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
							      return true;
							    }
next_after_fail2059:;
							  }
						      }
						    }
					          break;
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
	      case POINTER_DIFF_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _p0, 0))
		    {
		      {
			tree captures[5] ATTRIBUTE_UNUSED = { _q31, _p1, _p1, _q30, _p0 };
			if (TYPE_PRECISION (TREE_TYPE (captures[1])) >= TYPE_PRECISION (TREE_TYPE (captures[2]))
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2060;
			    {
			      res_op->set_op (NOP_EXPR, type, 1);
			      res_op->ops[0] = captures[3];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2060:;
			  }
		      }
		    }
	          break;
	        }
	      case NEGATE_EXPR:
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
			      case BIT_AND_EXPR:
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
							      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
							      {
 tree algn = wide_int_to_tree (TREE_TYPE (captures[0]), ~wi::to_wide (captures[1]));
								  gimple_seq *lseq = seq;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2061;
								  {
								    res_op->set_op (BIT_AND_EXPR, type, 2);
								    res_op->ops[0] = captures[0];
								    res_op->ops[1] =  algn;
								    res_op->resimplify (lseq, valueize);
								    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1012, __FILE__, __LINE__, true);
								    return true;
								  }
next_after_fail2061:;
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
  return false;
}

bool
gimple_simplify_EQ_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_211 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			if (gimple_simplify_211 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case CEIL_MOD_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_pow2p (_q21))
		    {
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_212 (res_op, seq, valueize, type, captures, EQ_EXPR, CEIL_MOD_EXPR))
			      return true;
			  }
		        }
		    }
	          break;
	        }
	      case FLOOR_MOD_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_pow2p (_q21))
		    {
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_212 (res_op, seq, valueize, type, captures, EQ_EXPR, FLOOR_MOD_EXPR))
			      return true;
			  }
		        }
		    }
	          break;
	        }
	      case ROUND_MOD_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_pow2p (_q21))
		    {
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_212 (res_op, seq, valueize, type, captures, EQ_EXPR, ROUND_MOD_EXPR))
			      return true;
			  }
		        }
		    }
	          break;
	        }
	      case TRUNC_MOD_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_pow2p (_q21))
		    {
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_212 (res_op, seq, valueize, type, captures, EQ_EXPR, TRUNC_MOD_EXPR))
			      return true;
			  }
		        }
		    }
	          break;
	        }
	      case LSHIFT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (gimple_zero_one_valued_p (_q20, valueize))
		    {
		      switch (TREE_CODE (_q21))
		        {
			case INTEGER_CST:
			  {
			    if (integer_zerop (_p1))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				  if (tree_fits_shwi_p (captures[1])
 && tree_to_shwi (captures[1]) > 0
 && tree_to_shwi (captures[1]) < TYPE_PRECISION (TREE_TYPE (captures[0]))
)
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2241;
				      {
					res_op->set_op (EQ_EXPR, type, 2);
					res_op->ops[0] = captures[0];
					res_op->ops[1] = captures[2];
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1081, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2241:;
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
	      case BIT_AND_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (gimple_truth_valued_p (_q20, valueize))
		    {
		      if (gimple_truth_valued_p (_q21, valueize))
			{
			  switch (TREE_CODE (_p1))
			    {
			    case SSA_NAME:
			      if (gimple *_d2 = get_def (valueize, _p1))
			        {
				  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				    switch (gimple_assign_rhs_code (_a2))
				      {
				      case BIT_XOR_EXPR:
				        {
					  tree _q50 = gimple_assign_rhs1 (_a2);
					  _q50 = do_valueize (valueize, _q50);
					  tree _q51 = gimple_assign_rhs2 (_a2);
					  _q51 = do_valueize (valueize, _q51);
					  if (tree_swap_operands_p (_q50, _q51))
					    std::swap (_q50, _q51);
					  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
					    {
					      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
						{
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2242;
						    {
						      res_op->set_op (BIT_NOT_EXPR, type, 1);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2242;
							res_op->ops[0] = _r1;
						      }
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2242:;
						  }
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2243;
						    {
						      res_op->set_op (BIT_NOT_EXPR, type, 1);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2243;
							res_op->ops[0] = _r1;
						      }
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2243:;
						  }
					        }
					    }
					  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
					    {
					      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
						{
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2244;
						    {
						      res_op->set_op (BIT_NOT_EXPR, type, 1);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2244;
							res_op->ops[0] = _r1;
						      }
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2244:;
						  }
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2245;
						    {
						      res_op->set_op (BIT_NOT_EXPR, type, 1);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2245;
							res_op->ops[0] = _r1;
						      }
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2245:;
						  }
					        }
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
	      case BIT_XOR_EXPR:
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
			      case BIT_AND_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if (gimple_truth_valued_p (_q50, valueize))
					{
					  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					    {
					      if (gimple_truth_valued_p (_q51, valueize))
						{
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2246;
						    {
						      res_op->set_op (BIT_NOT_EXPR, type, 1);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2246;
							res_op->ops[0] = _r1;
						      }
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2246:;
						  }
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2247;
						    {
						      res_op->set_op (BIT_NOT_EXPR, type, 1);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2247;
							res_op->ops[0] = _r1;
						      }
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2247:;
						  }
					        }
					    }
				        }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if (gimple_truth_valued_p (_q50, valueize))
					{
					  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					    {
					      if (gimple_truth_valued_p (_q51, valueize))
						{
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2248;
						    {
						      res_op->set_op (BIT_NOT_EXPR, type, 1);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2248;
							res_op->ops[0] = _r1;
						      }
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2248:;
						  }
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2249;
						    {
						      res_op->set_op (BIT_NOT_EXPR, type, 1);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[1];
							gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2249;
							res_op->ops[0] = _r1;
						      }
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2249:;
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
	      case BIT_NOT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if (gimple_truth_valued_p (_q20, valueize))
		    {
		      if (gimple_truth_valued_p (_p1, valueize))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_214 (res_op, seq, valueize, type, captures))
			      return true;
			  }
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
					if (gimple_simplify_215 (res_op, seq, valueize, type, captures, EQ_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_215 (res_op, seq, valueize, type, captures, EQ_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_215 (res_op, seq, valueize, type, captures, EQ_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_215 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					      if (gimple_simplify_216 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
									      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  switch (TREE_CODE (_q41))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q41))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
						    {
						      {
							tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q40, _q20 };
							if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
						  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
						    {
						      {
							tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q41, _q20 };
							if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				  if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q40, _q20 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
					  return true;
				      }
				    }
				  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q41, _q20 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				  case PLUS_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (tree_swap_operands_p (_q50, _q51))
				        std::swap (_q50, _q51);
				      switch (TREE_CODE (_q51))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _q51))
					    {
					      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					        switch (gimple_assign_rhs_code (_a3))
						  {
						  CASE_CONVERT:
						    {
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q20 };
							    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				      switch (TREE_CODE (_q50))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _q50))
					    {
					      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					        switch (gimple_assign_rhs_code (_a3))
						  {
						  CASE_CONVERT:
						    {
						      tree _q60 = gimple_assign_rhs1 (_a3);
						      _q60 = do_valueize (valueize, _q60);
						      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q51, _q20 };
							    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q20 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
					      return true;
					  }
				        }
				      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q51, _q20 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				if (gimple_simplify_220 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0])))
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2250;
					    {
					      res_op->set_op (EQ_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2250:;
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
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2251;
					    {
					      res_op->set_op (EQ_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2251:;
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
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2252;
					    {
					      res_op->set_op (EQ_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2252:;
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
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2253;
					    {
					      res_op->set_op (EQ_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2253:;
					  }
				      }
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
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
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
						  if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
						    {
						      {
							tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q40 };
							if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			      CASE_CONVERT:
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
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
						    {
						      {
							tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q30 };
							if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q30 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q40 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
			if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20 };
			if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
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
					if (gimple_simplify_221 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			      CASE_CONVERT:
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
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
						    {
						      {
							tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q30, _q21 };
							if (gimple_simplify_222 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q30, _q21 };
					if (gimple_simplify_222 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
					if (gimple_simplify_222 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
			if (gimple_simplify_222 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
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
					if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0])))
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2254;
					    {
					      res_op->set_op (EQ_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1028, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2254:;
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
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2255;
					    {
					      res_op->set_op (EQ_EXPR, type, 2);
					      res_op->ops[0] = captures[2];
					      res_op->ops[1] = captures[1];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1029, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2255:;
					  }
				      }
				    }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
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
					if (gimple_simplify_224 (res_op, seq, valueize, type, captures, EQ_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
					if (gimple_simplify_225 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
{
  tree _p0_pops[1];
  if (gimple_logical_inverted_value (_p0, _p0_pops, valueize))
    {
      tree _q20 = _p0_pops[0];
      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	{
	  if (gimple_truth_valued_p (_p1, valueize))
	    {
	      {
		tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		if (gimple_simplify_213 (res_op, seq, valueize, type, captures, EQ_EXPR))
		  return true;
	      }
	    }
        }
    }
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
		  case PLUS_EXPR:
		    {
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      tree _q31 = gimple_assign_rhs2 (_a1);
		      _q31 = do_valueize (valueize, _q31);
		      if (tree_swap_operands_p (_q30, _q31))
		        std::swap (_q30, _q31);
		      switch (TREE_CODE (_q31))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q31))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  CASE_CONVERT:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
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
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      if ((_q70 == _q50 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q50, 0) && types_match (_q70, _q50)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q50 };
							    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      switch (TREE_CODE (_q30))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q30))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  CASE_CONVERT:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
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
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q40 };
							    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				      if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q40 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      switch (TREE_CODE (_q31))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q31))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  CASE_CONVERT:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q50 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			  if (gimple *_d2 = get_def (valueize, _p1))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  CASE_CONVERT:
				    {
				      tree _q60 = gimple_assign_rhs1 (_a2);
				      _q60 = do_valueize (valueize, _q60);
				      if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
					      return true;
					  }
				        }
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
			    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
			      return true;
			  }
		        }
		      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30 };
			    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
			      return true;
			  }
		        }
		      break;
		    }
		  case POINTER_PLUS_EXPR:
		    {
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      tree _q31 = gimple_assign_rhs2 (_a1);
		      _q31 = do_valueize (valueize, _q31);
		      switch (TREE_CODE (_q30))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q30))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  CASE_CONVERT:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
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
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _q31 };
							    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				      if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _q31 };
					    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			  if (gimple *_d2 = get_def (valueize, _p1))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  CASE_CONVERT:
				    {
				      tree _q60 = gimple_assign_rhs1 (_a2);
				      _q60 = do_valueize (valueize, _q60);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
					    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
			    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
if (gimple_truth_valued_p (_p0, valueize))
  {
    switch (TREE_CODE (_p1))
      {
      case SSA_NAME:
        if (gimple *_d1 = get_def (valueize, _p1))
          {
	    if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	      switch (gimple_assign_rhs_code (_a1))
		{
		case BIT_NOT_EXPR:
		  {
		    tree _q30 = gimple_assign_rhs1 (_a1);
		    _q30 = do_valueize (valueize, _q30);
		    if (gimple_truth_valued_p (_q30, valueize))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q30, _p0 };
			  if (gimple_simplify_214 (res_op, seq, valueize, type, captures))
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
    tree _p1_pops[1];
    if (gimple_logical_inverted_value (_p1, _p1_pops, valueize))
      {
	tree _q30 = _p1_pops[0];
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      if (gimple_simplify_213 (res_op, seq, valueize, type, captures, EQ_EXPR))
	        return true;
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
	      case PLUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  switch (TREE_CODE (_q31))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q31))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _p0 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  switch (TREE_CODE (_q30))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q30))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _p0 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _p0 };
			if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _p0 };
			if (gimple_simplify_219 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
  tree _p1_pops[1];
  if (gimple_nop_convert (_p1, _p1_pops, valueize))
    {
      tree _q30 = _p1_pops[0];
      switch (TREE_CODE (_q30))
        {
	case SSA_NAME:
	  if (gimple *_d1 = get_def (valueize, _q30))
	    {
	      if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	        switch (gimple_assign_rhs_code (_a1))
		  {
		  case PLUS_EXPR:
		    {
		      tree _q40 = gimple_assign_rhs1 (_a1);
		      _q40 = do_valueize (valueize, _q40);
		      tree _q41 = gimple_assign_rhs2 (_a1);
		      _q41 = do_valueize (valueize, _q41);
		      if (tree_swap_operands_p (_q40, _q41))
		        std::swap (_q40, _q41);
		      switch (TREE_CODE (_q41))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q41))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  CASE_CONVERT:
				    {
				      tree _q60 = gimple_assign_rhs1 (_a2);
				      _q60 = do_valueize (valueize, _q60);
				      if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _p0 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      switch (TREE_CODE (_q40))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q40))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  CASE_CONVERT:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _p0 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _p0 };
			    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
			      return true;
			  }
		        }
		      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _p0 };
			    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			      case POINTER_PLUS_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
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
						  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
						    {
						      {
							tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q20, _q41 };
							if (gimple_simplify_222 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q20, _q41 };
					if (gimple_simplify_222 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
						  CASE_CONVERT:
						    {
						      tree _q60 = gimple_assign_rhs1 (_a3);
						      _q60 = do_valueize (valueize, _q60);
						      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q20, _q51 };
							    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q20, _q51 };
					    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case POINTER_PLUS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  switch (TREE_CODE (_q30))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q30))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _p0, _q31 };
					if (gimple_simplify_222 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _p0, _q31 };
			if (gimple_simplify_222 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
  tree _p1_pops[1];
  if (gimple_nop_convert (_p1, _p1_pops, valueize))
    {
      tree _q30 = _p1_pops[0];
      switch (TREE_CODE (_q30))
        {
	case SSA_NAME:
	  if (gimple *_d1 = get_def (valueize, _q30))
	    {
	      if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	        switch (gimple_assign_rhs_code (_a1))
		  {
		  case POINTER_PLUS_EXPR:
		    {
		      tree _q40 = gimple_assign_rhs1 (_a1);
		      _q40 = do_valueize (valueize, _q40);
		      tree _q41 = gimple_assign_rhs2 (_a1);
		      _q41 = do_valueize (valueize, _q41);
		      switch (TREE_CODE (_q40))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _q40))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  CASE_CONVERT:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _p0, _q41 };
					    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _p0, _q41 };
			    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
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
				    if (gimple_simplify_227 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			      case ADDR_EXPR:
			        {
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q21 };
				    if (gimple_simplify_227 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			  tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q21 };
			  if (gimple_simplify_227 (res_op, seq, valueize, type, captures, EQ_EXPR))
			    return true;
			}
		        break;
		      }
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
								    if (gimple_simplify_228 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
							  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51 };
							  if (gimple_simplify_228 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
							  if (gimple_simplify_228 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
						tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51 };
						if (gimple_simplify_228 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			  if (gimple_simplify_227 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case TRUNC_DIV_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_229 (res_op, seq, valueize, type, captures, EQ_EXPR, LT_EXPR))
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
					      case MINUS_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
						  switch (TREE_CODE (_q50))
						    {
						    case INTEGER_CST:
						      {
							switch (TREE_CODE (_q51))
							  {
							  case SSA_NAME:
							    if (gimple *_d4 = get_def (valueize, _q51))
							      {
								if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								  switch (gimple_assign_rhs_code (_a4))
								    {
								    CASE_CONVERT:
								      {
									tree _q70 = gimple_assign_rhs1 (_a4);
									_q70 = do_valueize (valueize, _q70);
									if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
									  {
									    {
									      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
									      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			      case MINUS_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  switch (TREE_CODE (_q30))
				    {
				    case INTEGER_CST:
				      {
					switch (TREE_CODE (_q31))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _q31))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
						    {
						    CASE_CONVERT:
						      {
							tree _q50 = gimple_assign_rhs1 (_a3);
							_q50 = do_valueize (valueize, _q50);
							switch (TREE_CODE (_p1))
							  {
							  case SSA_NAME:
							    if (gimple *_d4 = get_def (valueize, _p1))
							      {
								if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								  switch (gimple_assign_rhs_code (_a4))
								    {
								    CASE_CONVERT:
								      {
									tree _q70 = gimple_assign_rhs1 (_a4);
									_q70 = do_valueize (valueize, _q70);
									if ((_q70 == _q50 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q50, 0) && types_match (_q70, _q50)))
									  {
									    {
									      tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q30 };
									      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
							if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
							  {
							    {
							      tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q30 };
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					    if (gimple *_d3 = get_def (valueize, _p1))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
						    {
						    CASE_CONVERT:
						      {
							tree _q60 = gimple_assign_rhs1 (_a3);
							_q60 = do_valueize (valueize, _q60);
							if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
							  {
							    {
							      tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					      case MINUS_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
						  switch (TREE_CODE (_q50))
						    {
						    case INTEGER_CST:
						      {
							if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
							  {
							    {
							      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			      case MINUS_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  switch (TREE_CODE (_q40))
				    {
				    case INTEGER_CST:
				      {
					switch (TREE_CODE (_q41))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _q41))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
						    {
						    CASE_CONVERT:
						      {
							tree _q60 = gimple_assign_rhs1 (_a3);
							_q60 = do_valueize (valueize, _q60);
							if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
							  {
							    {
							      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case MINUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q20))
		    {
		    case INTEGER_CST:
		      {
			switch (TREE_CODE (_q21))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _q21))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    CASE_CONVERT:
				      {
					tree _q40 = gimple_assign_rhs1 (_a2);
					_q40 = do_valueize (valueize, _q40);
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
							if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
							  {
							    {
							      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			    if (gimple *_d2 = get_def (valueize, _p1))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    CASE_CONVERT:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			      case MINUS_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  switch (TREE_CODE (_q40))
				    {
				    case INTEGER_CST:
				      {
					switch (TREE_CODE (_q41))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _q41))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
						    {
						    CASE_CONVERT:
						      {
							tree _q60 = gimple_assign_rhs1 (_a3);
							_q60 = do_valueize (valueize, _q60);
							if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
							  {
							    {
							      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			switch (TREE_CODE (_q31))
			  {
			  case SSA_NAME:
			    if (gimple *_d2 = get_def (valueize, _q31))
			      {
				if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				  switch (gimple_assign_rhs_code (_a2))
				    {
				    CASE_CONVERT:
				      {
					tree _q50 = gimple_assign_rhs1 (_a2);
					_q50 = do_valueize (valueize, _q50);
					if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case LT_EXPR:
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
			      case GT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR, EQ_EXPR))
					      return true;
					  }
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR, EQ_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case EQ_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, EQ_EXPR, GT_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, EQ_EXPR, GT_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case NE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, LE_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, LE_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case LT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR, EQ_EXPR))
					      return true;
					  }
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR, EQ_EXPR))
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
	      case LE_EXPR:
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
			      case GE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR, EQ_EXPR))
					      return true;
					  }
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR, EQ_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case EQ_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, GE_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, GE_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case NE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, LT_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, LT_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case LE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR, EQ_EXPR))
					      return true;
					  }
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR, EQ_EXPR))
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
	      case GT_EXPR:
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
			      case GT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR, EQ_EXPR))
					      return true;
					  }
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR, EQ_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case EQ_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, EQ_EXPR, GT_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, EQ_EXPR, GT_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case NE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, LE_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, LE_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case LT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR, EQ_EXPR))
					      return true;
					  }
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR, EQ_EXPR))
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
	      case GE_EXPR:
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
			      case GE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR, EQ_EXPR))
					      return true;
					  }
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR, EQ_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case EQ_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, GE_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, GE_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case NE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, LT_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, LT_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case LE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR, EQ_EXPR))
					      return true;
					  }
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR, EQ_EXPR))
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
	      case EQ_EXPR:
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
			      case LT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, EQ_EXPR, GT_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, EQ_EXPR, GT_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case LE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, GE_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, GE_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case GT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, EQ_EXPR, GT_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, EQ_EXPR, GT_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case GE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, GE_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, GE_EXPR))
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
	      case NE_EXPR:
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
			      case LT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, LE_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, LE_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case LE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, LT_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, LT_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case GT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, LE_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, LE_EXPR))
					      return true;
					  }
				        }
				    }
			          break;
			        }
			      case GE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, LT_EXPR))
					      return true;
					  }
				        }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
					    if (gimple_simplify_232 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, LT_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, EQ_EXPR, LE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, EQ_EXPR, LE_EXPR))
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
				if (gimple_simplify_234 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, EQ_EXPR, GE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, EQ_EXPR, GE_EXPR))
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
				if (gimple_simplify_235 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
{
  tree _p0_pops[1];
  if (gimple_with_possible_nonzero_bits2 (_p0, _p0_pops, valueize))
    {
      tree _q20 = _p0_pops[0];
      {
	tree _p1_pops[1];
	if (gimple_with_certain_nonzero_bits2 (_p1, _p1_pops, valueize))
	  {
	    tree _q40 = _p1_pops[0];
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
	      if (gimple_simplify_231 (res_op, seq, valueize, type, captures, EQ_EXPR))
	        return true;
	    }
	  }
      }
    }
}
{
  tree _p0_pops[1];
  if (gimple_with_certain_nonzero_bits2 (_p0, _p0_pops, valueize))
    {
      tree _q20 = _p0_pops[0];
      {
	tree _p1_pops[1];
	if (gimple_with_possible_nonzero_bits2 (_p1, _p1_pops, valueize))
	  {
	    tree _q40 = _p1_pops[0];
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
	      if (gimple_simplify_231 (res_op, seq, valueize, type, captures, EQ_EXPR))
	        return true;
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, EQ_EXPR, LE_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, EQ_EXPR, LE_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, EQ_EXPR, GE_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, EQ_EXPR, GE_EXPR))
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
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case LSHIFT_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (integer_pow2p (_q30))
				    {
				      if (integer_pow2p (_q21))
					{
					  if (integer_zerop (_p1))
					    {
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q21 };
						if (gimple_simplify_236 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR))
						  return true;
					      }
					    }
				        }
				    }
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
						      if (gimple_simplify_237 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				  if (integer_onep (_q30))
				    {
				      if (integer_pow2p (_q21))
					{
					  if (integer_zerop (_p1))
					    {
					      {
						tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q21 };
						if (gimple_simplify_238 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR))
						  return true;
					      }
					    }
				        }
				    }
			          break;
			        }
			      case RSHIFT_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (integer_pow2p (_q30))
				    {
				      if (integer_pow2p (_q21))
					{
					  if (integer_zerop (_p1))
					    {
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q21 };
						if (gimple_simplify_239 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR))
						  return true;
					      }
					    }
				        }
				    }
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
						      if (gimple_simplify_240 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
		    }
		{
		  tree _q20_pops[1];
		  if (gimple_nop_convert (_q20, _q20_pops, valueize))
		    {
		      tree _q30 = _q20_pops[0];
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
					  if (integer_pow2p (_q21))
					    {
					      if (integer_zerop (_p1))
						{
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q41, _q21 };
						    if (gimple_simplify_238 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR))
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
			  break;
		        default:;
		        }
		    }
		}
	          break;
	        }
	      case RSHIFT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_onep (_q20))
		    {
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			    if (gimple_simplify_241 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR))
			      return true;
			  }
		        }
		    }
	          break;
	        }
	      case LSHIFT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
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
				if (gimple_simplify_242 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case LROTATE_EXPR:
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
			      case LROTATE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					if (gimple_simplify_243 (res_op, seq, valueize, type, captures, LROTATE_EXPR, RROTATE_EXPR, EQ_EXPR))
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
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_244 (res_op, seq, valueize, type, captures, LROTATE_EXPR, RROTATE_EXPR, EQ_EXPR))
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
		  switch (TREE_CODE (_p1))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			  if (gimple_simplify_245 (res_op, seq, valueize, type, captures, LROTATE_EXPR, RROTATE_EXPR, EQ_EXPR))
			    return true;
			}
		        break;
		      }
		    default:;
		    }
	          break;
	        }
	      case RROTATE_EXPR:
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
			      case RROTATE_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					if (gimple_simplify_243 (res_op, seq, valueize, type, captures, RROTATE_EXPR, LROTATE_EXPR, EQ_EXPR))
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
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_244 (res_op, seq, valueize, type, captures, RROTATE_EXPR, LROTATE_EXPR, EQ_EXPR))
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
		  switch (TREE_CODE (_p1))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			  if (gimple_simplify_245 (res_op, seq, valueize, type, captures, RROTATE_EXPR, LROTATE_EXPR, EQ_EXPR))
			    return true;
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case TRUNC_DIV_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if ((_q21 == _q31 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q31, 0) && types_match (_q21, _q31)))
				    {
				      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
					    if (gimple_simplify_246 (res_op, seq, valueize, type, captures))
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
		  switch (TREE_CODE (_q21))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case TRUNC_DIV_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
				    {
				      if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q40, _q20 };
					    if (gimple_simplify_246 (res_op, seq, valueize, type, captures))
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
		  switch (TREE_CODE (_q30))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q30))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case TRUNC_DIV_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
				    {
				      if ((_q31 == _q41 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _q41, 0) && types_match (_q31, _q41)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q41 };
					    if (gimple_simplify_246 (res_op, seq, valueize, type, captures))
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
		  switch (TREE_CODE (_q31))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q31))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case TRUNC_DIV_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
				    {
				      if ((_q51 == _q30 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q30, 0) && types_match (_q51, _q30)))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q31, _p0, _q30 };
					    if (gimple_simplify_246 (res_op, seq, valueize, type, captures))
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
 && (TREE_CODE_CLASS (EQ_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2256;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2256;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2256;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2256:;
					  }
				      }
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
 && (TREE_CODE_CLASS (EQ_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2257;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2257;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2257;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2257:;
		      }
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
		    switch (TREE_CODE (_p1))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _p1))
		          {
			    if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			      switch (gimple_call_combined_fn (_c2))
				{
				case CFN_BUILT_IN_BSWAP128:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
					if (gimple_simplify_247 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_BSWAP128))
					  return true;
				      }
				    }
				  break;
				default:;
				}
		          }
		        break;
		      case INTEGER_CST:
		        {
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			    if (gimple_simplify_248 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_BSWAP128))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP16:
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
				case CFN_BUILT_IN_BSWAP16:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
					if (gimple_simplify_247 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_BSWAP16))
					  return true;
				      }
				    }
				  break;
				default:;
				}
		          }
		        break;
		      case INTEGER_CST:
		        {
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			    if (gimple_simplify_248 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_BSWAP16))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP32:
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
				case CFN_BUILT_IN_BSWAP32:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
					if (gimple_simplify_247 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_BSWAP32))
					  return true;
				      }
				    }
				  break;
				default:;
				}
		          }
		        break;
		      case INTEGER_CST:
		        {
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			    if (gimple_simplify_248 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_BSWAP32))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP64:
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
				case CFN_BUILT_IN_BSWAP64:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
					if (gimple_simplify_247 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_BSWAP64))
					  return true;
				      }
				    }
				  break;
				default:;
				}
		          }
		        break;
		      case INTEGER_CST:
		        {
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			    if (gimple_simplify_248 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_BSWAP64))
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
 && (TREE_CODE_CLASS (EQ_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2258;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2258;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2258;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2258:;
		      }
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
	      case COND_EXPR:
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
			      case COND_EXPR:
			        {
				  tree _q60 = gimple_assign_rhs1 (_a2);
				  _q60 = do_valueize (valueize, _q60);
				  tree _q61 = gimple_assign_rhs2 (_a2);
				  _q61 = do_valueize (valueize, _q61);
				  tree _q62 = gimple_assign_rhs3 (_a2);
				  _q62 = do_valueize (valueize, _q62);
				  if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
				    {
				      if ((_q62 == _q22 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q22, 0) && types_match (_q62, _q22)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _q60 };
					    const enum tree_code eqne = EQ_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2259;
						{
						  res_op->set_op (COND_EXPR, type, 3);
						  {
						    tree _o1[2], _r1;
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[0];
						      if (COMPARISON_CLASS_P (_o2[0]))
						        {
						          if (!seq) return false;
						          _o2[0] = gimple_build (seq, TREE_CODE (_o2[0]), TREE_TYPE (_o2[0]), TREE_OPERAND (_o2[0], 0), TREE_OPERAND (_o2[0], 1));
						        }
						      _o2[1] = captures[3];
						      if (COMPARISON_CLASS_P (_o2[1]))
						        {
						          if (!seq) return false;
						          _o2[1] = gimple_build (seq, TREE_CODE (_o2[1]), TREE_TYPE (_o2[1]), TREE_OPERAND (_o2[1], 0), TREE_OPERAND (_o2[1], 1));
						        }
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2259;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2259;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2259;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1082, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2259:;
					      }
					  }
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q21, _q22, _q20 };
					    const enum tree_code eqne = EQ_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2260;
						{
						  res_op->set_op (COND_EXPR, type, 3);
						  {
						    tree _o1[2], _r1;
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[0];
						      if (COMPARISON_CLASS_P (_o2[0]))
						        {
						          if (!seq) return false;
						          _o2[0] = gimple_build (seq, TREE_CODE (_o2[0]), TREE_TYPE (_o2[0]), TREE_OPERAND (_o2[0], 0), TREE_OPERAND (_o2[0], 1));
						        }
						      _o2[1] = captures[3];
						      if (COMPARISON_CLASS_P (_o2[1]))
						        {
						          if (!seq) return false;
						          _o2[1] = gimple_build (seq, TREE_CODE (_o2[1]), TREE_TYPE (_o2[1]), TREE_OPERAND (_o2[1], 0), TREE_OPERAND (_o2[1], 1));
						        }
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2260;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2260;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2260;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1082, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2260:;
					      }
					  }
				        }
				    }
				  if ((_q61 == _q22 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q22, 0) && types_match (_q61, _q22)))
				    {
				      if ((_q62 == _q21 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q21, 0) && types_match (_q62, _q21)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _q60 };
					    const enum tree_code eqne = EQ_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2261;
						{
						  res_op->set_op (COND_EXPR, type, 3);
						  {
						    tree _o1[2], _r1;
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[0];
						      if (COMPARISON_CLASS_P (_o2[0]))
						        {
						          if (!seq) return false;
						          _o2[0] = gimple_build (seq, TREE_CODE (_o2[0]), TREE_TYPE (_o2[0]), TREE_OPERAND (_o2[0], 0), TREE_OPERAND (_o2[0], 1));
						        }
						      _o2[1] = captures[3];
						      if (COMPARISON_CLASS_P (_o2[1]))
						        {
						          if (!seq) return false;
						          _o2[1] = gimple_build (seq, TREE_CODE (_o2[1]), TREE_TYPE (_o2[1]), TREE_OPERAND (_o2[1], 0), TREE_OPERAND (_o2[1], 1));
						        }
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2261;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2261;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2261;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1083, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2261:;
					      }
					  }
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q22, _q21, _q20 };
					    const enum tree_code eqne = EQ_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2262;
						{
						  res_op->set_op (COND_EXPR, type, 3);
						  {
						    tree _o1[2], _r1;
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[0];
						      if (COMPARISON_CLASS_P (_o2[0]))
						        {
						          if (!seq) return false;
						          _o2[0] = gimple_build (seq, TREE_CODE (_o2[0]), TREE_TYPE (_o2[0]), TREE_OPERAND (_o2[0], 0), TREE_OPERAND (_o2[0], 1));
						        }
						      _o2[1] = captures[3];
						      if (COMPARISON_CLASS_P (_o2[1]))
						        {
						          if (!seq) return false;
						          _o2[1] = gimple_build (seq, TREE_CODE (_o2[1]), TREE_TYPE (_o2[1]), TREE_OPERAND (_o2[1], 0), TREE_OPERAND (_o2[1], 1));
						        }
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2262;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2262;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2262;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1083, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2262:;
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
				  if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
				    {
				      if ((_q62 == _q22 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q22, 0) && types_match (_q62, _q22)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _q60 };
					    const enum tree_code eqne = EQ_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2263;
						{
						  res_op->set_op (VEC_COND_EXPR, type, 3);
						  {
						    tree _o1[2], _r1;
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[0];
						      _o2[1] = captures[3];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2263;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2263;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2263;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1082, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2263:;
					      }
					  }
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q21, _q22, _q20 };
					    const enum tree_code eqne = EQ_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2264;
						{
						  res_op->set_op (VEC_COND_EXPR, type, 3);
						  {
						    tree _o1[2], _r1;
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[0];
						      _o2[1] = captures[3];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2264;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2264;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2264;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1082, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2264:;
					      }
					  }
				        }
				    }
				  if ((_q61 == _q22 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q22, 0) && types_match (_q61, _q22)))
				    {
				      if ((_q62 == _q21 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q21, 0) && types_match (_q62, _q21)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _q60 };
					    const enum tree_code eqne = EQ_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2265;
						{
						  res_op->set_op (VEC_COND_EXPR, type, 3);
						  {
						    tree _o1[2], _r1;
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[0];
						      _o2[1] = captures[3];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2265;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2265;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2265;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1083, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2265:;
					      }
					  }
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q22, _q21, _q20 };
					    const enum tree_code eqne = EQ_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2266;
						{
						  res_op->set_op (VEC_COND_EXPR, type, 3);
						  {
						    tree _o1[2], _r1;
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[0];
						      _o2[1] = captures[3];
						      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2266;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2266;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2266;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1083, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2266:;
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
	      case LT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, LT_EXPR, GE_EXPR, UNGE_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, LT_EXPR, GE_EXPR, UNGE_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case LE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, LE_EXPR, GT_EXPR, UNGT_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, LE_EXPR, GT_EXPR, UNGT_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case EQ_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR, NE_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR, NE_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case NE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR, EQ_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR, EQ_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case GE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, GE_EXPR, LT_EXPR, UNLT_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, GE_EXPR, LT_EXPR, UNLT_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case GT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, GT_EXPR, LE_EXPR, UNLE_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, GT_EXPR, LE_EXPR, UNLE_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case UNORDERED_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, UNORDERED_EXPR, ORDERED_EXPR, ORDERED_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, UNORDERED_EXPR, ORDERED_EXPR, ORDERED_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case ORDERED_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, ORDERED_EXPR, UNORDERED_EXPR, UNORDERED_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, ORDERED_EXPR, UNORDERED_EXPR, UNORDERED_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case UNLT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, UNLT_EXPR, GE_EXPR, GE_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, UNLT_EXPR, GE_EXPR, GE_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case UNLE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, UNLE_EXPR, GT_EXPR, GT_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, UNLE_EXPR, GT_EXPR, GT_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case UNGT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, UNGT_EXPR, LE_EXPR, LE_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, UNGT_EXPR, LE_EXPR, LE_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case UNGE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, UNGE_EXPR, LT_EXPR, LT_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, UNGE_EXPR, LT_EXPR, LT_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case UNEQ_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, UNEQ_EXPR, LTGT_EXPR, LTGT_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, UNEQ_EXPR, LTGT_EXPR, LTGT_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case LTGT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_249 (res_op, seq, valueize, type, captures, LTGT_EXPR, UNEQ_EXPR, UNEQ_EXPR))
			  return true;
		      }
		    }
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_250 (res_op, seq, valueize, type, captures, LTGT_EXPR, UNEQ_EXPR, UNEQ_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case MINUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_251 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case POINTER_DIFF_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_252 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				if (gimple_simplify_254 (res_op, seq, valueize, type, captures, EQ_EXPR))
				  return true;
			      }
			      break;
			    }
		          default:;
		          }
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
			    if (gimple_simplify_253 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
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
	if (gimple_simplify_255 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case BIT_NOT_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_256 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case BIT_NOT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_256 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
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
			      case BIT_NOT_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _p1, _q40 };
				    if (gimple_simplify_257 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					if (gimple_simplify_257 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		      if (gimple_simplify_258 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
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
					if (gimple_simplify_259 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					    if (gimple_simplify_259 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			  if (gimple_simplify_258 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
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
	  if (gimple_simplify_260 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, PLUS_EXPR, EQ_EXPR))
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
		    case REAL_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case REAL_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, MINUS_EXPR, EQ_EXPR))
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
				if (gimple_simplify_262 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
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
			  if (gimple_simplify_266 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
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
		  switch (TREE_CODE (_p1))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			  if (gimple_simplify_267 (res_op, seq, valueize, type, captures, EQ_EXPR))
			    return true;
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
		        return true;
		    }
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if (zerop (_p1))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			const enum tree_code eqne = EQ_EXPR;
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2267;
			{
			  res_op->set_op (eqne, type, 2);
			  res_op->ops[0] = captures[0];
			  res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[0]));
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1084, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2267:;
		      }
		    }
	          break;
	        }
	      case ABSU_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if (zerop (_p1))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			const enum tree_code eqne = EQ_EXPR;
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2268;
			{
			  res_op->set_op (eqne, type, 2);
			  res_op->ops[0] = captures[0];
			  res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[0]));
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1084, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2268:;
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
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
				    if (gimple_simplify_270 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		    if (gimple_simplify_271 (res_op, seq, valueize, type, captures, EQ_EXPR))
		      return true;
		  }
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
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
						tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _p1 };
						if (gimple_simplify_272 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case ADDR_EXPR:
	        {
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			  if (gimple_simplify_273 (res_op, seq, valueize, type, captures, EQ_EXPR))
			    return true;
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_272 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_274 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_275 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_275 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, EQ_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, EQ_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, EQ_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, EQ_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, EQ_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, EQ_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_SQRT, EQ_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_SQRT, EQ_EXPR))
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
    case ADDR_EXPR:
      {
	switch (TREE_CODE (_p1))
	  {
	  case SSA_NAME:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		if (gimple_simplify_273 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case BIT_XOR_EXPR:
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
			if (gimple_simplify_275 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_275 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case BIT_AND_EXPR:
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
			if (gimple_simplify_276 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_276 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case BIT_AND_EXPR:
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
			if (gimple_simplify_276 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_276 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
							switch (TREE_CODE (_p1))
							  {
							  case SSA_NAME:
							    if (gimple *_d4 = get_def (valueize, _p1))
							      {
								if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								  switch (gimple_assign_rhs_code (_a4))
								    {
								    CASE_CONVERT:
								      {
									tree _q70 = gimple_assign_rhs1 (_a4);
									_q70 = do_valueize (valueize, _q70);
									if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
									  {
									    {
									      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q31 };
									      if (gimple_simplify_277 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					      case BIT_AND_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
						  if (tree_swap_operands_p (_q50, _q51))
						    std::swap (_q50, _q51);
						  switch (TREE_CODE (_q50))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q50))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      CASE_CONVERT:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
								    {
								      switch (TREE_CODE (_q51))
								        {
									case INTEGER_CST:
									  {
									    {
									      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q50, _q20, _q51 };
									      if (gimple_simplify_277 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_278 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_278 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case BIT_IOR_EXPR:
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
			tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
			if (gimple_simplify_278 (res_op, seq, valueize, type, captures, EQ_EXPR))
			  return true;
		      }
		    }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q31, _p0 };
			if (gimple_simplify_278 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
						tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _p1 };
						if (gimple_simplify_279 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_279 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
{
  tree _p0_pops[1];
  if (gimple_nop_convert (_p0, _p0_pops, valueize))
    {
      tree _q20 = _p0_pops[0];
      if (integer_zerop (_p1))
	{
	  {
	    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	    if (gimple_simplify_280 (res_op, seq, valueize, type, captures, EQ_EXPR))
	      return true;
	  }
        }
    }
}
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_280 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
	      case BIT_AND_EXPR:
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
			      case BIT_AND_EXPR:
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
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
					if (gimple_simplify_281 (res_op, seq, valueize, type, captures, EQ_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
					if (gimple_simplify_281 (res_op, seq, valueize, type, captures, EQ_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
					if (gimple_simplify_281 (res_op, seq, valueize, type, captures, EQ_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
					if (gimple_simplify_281 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  if (integer_pow2p (_q21))
		    {
		      if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			    if (gimple_simplify_282 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR))
			      return true;
			  }
		        }
		    }
	          break;
	        }
	      case GE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_zerop (_q21))
		    {
		      switch (TREE_CODE (_p1))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _p1))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  case GE_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (integer_zerop (_q51))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					    if (gimple_simplify_283 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR, GE_EXPR))
					      return true;
					  }
				        }
				      break;
				    }
				  case LT_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (integer_zerop (_q51))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q20 };
					    if (gimple_simplify_284 (res_op, seq, valueize, type, captures, EQ_EXPR, LT_EXPR))
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
	          break;
	        }
	      case LT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_zerop (_q21))
		    {
		      switch (TREE_CODE (_p1))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _p1))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  case LT_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (integer_zerop (_q51))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					    if (gimple_simplify_283 (res_op, seq, valueize, type, captures, LT_EXPR, EQ_EXPR, GE_EXPR))
					      return true;
					  }
				        }
				      break;
				    }
				  case GE_EXPR:
				    {
				      tree _q50 = gimple_assign_rhs1 (_a2);
				      _q50 = do_valueize (valueize, _q50);
				      tree _q51 = gimple_assign_rhs2 (_a2);
				      _q51 = do_valueize (valueize, _q51);
				      if (integer_zerop (_q51))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					    if (gimple_simplify_285 (res_op, seq, valueize, type, captures, EQ_EXPR, LT_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      CASE_CONVERT:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  if (integer_pow2p (_q21))
				    {
				      if (integer_zerop (_p1))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q21 };
					    if (gimple_simplify_287 (res_op, seq, valueize, type, captures, EQ_EXPR, GE_EXPR))
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
		  if (integer_pow2p (_q21))
		    {
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q20, _q21 };
			    if (gimple_simplify_288 (res_op, seq, valueize, type, captures, EQ_EXPR, GE_EXPR))
			      return true;
			  }
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
								    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
						    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
						if (gimple_simplify_289 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				if (gimple_simplify_289 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
						    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				if (gimple_simplify_290 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		if (gimple_simplify_290 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_291 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
		  switch (TREE_CODE (_p1))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			  if (gimple_simplify_292 (res_op, seq, valueize, type, captures, EQ_EXPR))
			    return true;
			}
		        break;
		      }
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
			      case NE_EXPR:
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
					if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
					      if (gimple_simplify_293 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR))
					        return true;
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
					if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
					      if (gimple_simplify_293 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
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
			      case NE_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
				    {
				      switch (TREE_CODE (_q41))
				        {
					case INTEGER_CST:
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
					      if (gimple_simplify_293 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR))
					        return true;
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
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
				    {
				      switch (TREE_CODE (_q41))
				        {
					case INTEGER_CST:
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
					      if (gimple_simplify_293 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
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
if (integer_onep (_p1))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (TREE_CODE (TREE_TYPE (captures[0])) == BOOLEAN_TYPE
 && types_match (type, TREE_TYPE (captures[0]))
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2269;
	  {
	    tree tem;
	    tem = captures[0];
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1085, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail2269:;
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
			if (integer_zerop (_p1))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			      const enum tree_code cmp = EQ_EXPR;
			      const enum tree_code icmp = LE_EXPR;
			      {
 tree csts = bitmask_inv_cst_vector_p (captures[2]);
				  if (csts && (VECTOR_TYPE_P (TREE_TYPE (captures[2])) || single_use (captures[0]))
)
				    {
				      {
 auto optab = VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 ? optab_vector : optab_default;
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
					  if (target_supports_op_p (utype, icmp, optab)
 || (optimize_vectors_before_lowering_p ()
 && (!target_supports_op_p (type, cmp, optab)
 || !target_supports_op_p (type, BIT_AND_EXPR, optab)))
)
					    {
					      if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2270;
						  {
						    res_op->set_op (icmp, type, 2);
						    res_op->ops[0] = captures[1];
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1086, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2270:;
						}
					      else
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2271;
						  {
						    res_op->set_op (icmp, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2271;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1087, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2271:;
						}
					    }
				      }
				    }
			      }
			    }
			  }
		        break;
		      }
		    case INTEGER_CST:
		      {
			if (integer_zerop (_p1))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			      const enum tree_code cmp = EQ_EXPR;
			      const enum tree_code icmp = LE_EXPR;
			      {
 tree csts = bitmask_inv_cst_vector_p (captures[2]);
				  if (csts && (VECTOR_TYPE_P (TREE_TYPE (captures[2])) || single_use (captures[0]))
)
				    {
				      {
 auto optab = VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 ? optab_vector : optab_default;
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
					  if (target_supports_op_p (utype, icmp, optab)
 || (optimize_vectors_before_lowering_p ()
 && (!target_supports_op_p (type, cmp, optab)
 || !target_supports_op_p (type, BIT_AND_EXPR, optab)))
)
					    {
					      if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2272;
						  {
						    res_op->set_op (icmp, type, 2);
						    res_op->ops[0] = captures[1];
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1086, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2272:;
						}
					      else
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2273;
						  {
						    res_op->set_op (icmp, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2273;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1087, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2273:;
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
		  switch (TREE_CODE (_q20))
		    {
		    case VECTOR_CST:
		      {
			if (integer_zerop (_p1))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			      const enum tree_code cmp = EQ_EXPR;
			      const enum tree_code icmp = LE_EXPR;
			      {
 tree csts = bitmask_inv_cst_vector_p (captures[2]);
				  if (csts && (VECTOR_TYPE_P (TREE_TYPE (captures[2])) || single_use (captures[0]))
)
				    {
				      {
 auto optab = VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 ? optab_vector : optab_default;
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
					  if (target_supports_op_p (utype, icmp, optab)
 || (optimize_vectors_before_lowering_p ()
 && (!target_supports_op_p (type, cmp, optab)
 || !target_supports_op_p (type, BIT_AND_EXPR, optab)))
)
					    {
					      if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2274;
						  {
						    res_op->set_op (icmp, type, 2);
						    res_op->ops[0] = captures[1];
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1086, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2274:;
						}
					      else
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2275;
						  {
						    res_op->set_op (icmp, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2275;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1087, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2275:;
						}
					    }
				      }
				    }
			      }
			    }
			  }
		        break;
		      }
		    case INTEGER_CST:
		      {
			if (integer_zerop (_p1))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			      const enum tree_code cmp = EQ_EXPR;
			      const enum tree_code icmp = LE_EXPR;
			      {
 tree csts = bitmask_inv_cst_vector_p (captures[2]);
				  if (csts && (VECTOR_TYPE_P (TREE_TYPE (captures[2])) || single_use (captures[0]))
)
				    {
				      {
 auto optab = VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 ? optab_vector : optab_default;
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
					  if (target_supports_op_p (utype, icmp, optab)
 || (optimize_vectors_before_lowering_p ()
 && (!target_supports_op_p (type, cmp, optab)
 || !target_supports_op_p (type, BIT_AND_EXPR, optab)))
)
					    {
					      if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2276;
						  {
						    res_op->set_op (icmp, type, 2);
						    res_op->ops[0] = captures[1];
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1086, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2276:;
						}
					      else
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2277;
						  {
						    res_op->set_op (icmp, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2277;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1087, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2277:;
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
	      case RSHIFT_EXPR:
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
			      case MULT_EXPR:
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
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q31))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      CASE_CONVERT:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  switch (TREE_CODE (_q21))
								    {
								    case INTEGER_CST:
								      {
									if (integer_zerop (_p1))
									  {
									    {
									      tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q60, _q21 };
									      if (gimple_simplify_294 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
				if (gimple_simplify_295 (res_op, seq, valueize, type, captures, PLUS_EXPR, MINUS_EXPR, EQ_EXPR))
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
				if (gimple_simplify_295 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR, EQ_EXPR))
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
				if (gimple_simplify_296 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR))
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
	      case CFN_BUILT_IN_CLZ:
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, EQ_EXPR, LT_EXPR, CFN_BUILT_IN_CLZ))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_CTZ))
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_FFS))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_REDUC_IOR:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    if (integer_zerop (_p1))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			  if (gimple_simplify_303 (res_op, seq, valueize, type, captures, EQ_EXPR))
			    return true;
			}
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_FFSIMAX))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNT))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_CLZL:
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, EQ_EXPR, LT_EXPR, CFN_BUILT_IN_CLZL))
			      return true;
			  }
		          break;
		        }
		      default:;
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_CTZL))
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_FFSL))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNTLL))
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_CTZIMAX))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR, CFN_POPCOUNT))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_CLZ:
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, EQ_EXPR, LT_EXPR, CFN_CLZ))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
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
			    if (gimple_simplify_298 (res_op, seq, valueize, type, captures, EQ_EXPR, LT_EXPR))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_CTZ:
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_CTZ))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
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
			    if (gimple_simplify_300 (res_op, seq, valueize, type, captures, EQ_EXPR))
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_FFS))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNTIMAX))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_CLZIMAX:
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, EQ_EXPR, LT_EXPR, CFN_BUILT_IN_CLZIMAX))
			      return true;
			  }
		          break;
		        }
		      default:;
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, EQ_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNTL))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_CLZLL:
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, EQ_EXPR, LT_EXPR, CFN_BUILT_IN_CLZLL))
			      return true;
			  }
		          break;
		        }
		      default:;
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_CTZLL))
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, EQ_EXPR, CFN_BUILT_IN_FFSLL))
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
gimple_simplify_LE_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
			      if (gimple_simplify_347 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR))
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
			if (gimple_simplify_348 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, LE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, LE_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, LE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, LE_EXPR))
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
			      if (gimple_simplify_374 (res_op, seq, valueize, type, captures, LE_EXPR, GT_EXPR))
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
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, LE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, LE_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, LE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					      if (gimple_simplify_351 (res_op, seq, valueize, type, captures, LE_EXPR))
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
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, LE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, LE_EXPR))
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
									      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, LE_EXPR))
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
				if (gimple_simplify_220 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					if (gimple_simplify_353 (res_op, seq, valueize, type, captures, LE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_354 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					if (gimple_simplify_224 (res_op, seq, valueize, type, captures, LE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
					if (gimple_simplify_225 (res_op, seq, valueize, type, captures, LE_EXPR))
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
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, GE_EXPR))
			  return true;
		      }
		    }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, GE_EXPR))
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
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, LE_EXPR))
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
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, GE_EXPR))
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
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, LE_EXPR, BIT_AND_EXPR))
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
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, LE_EXPR, BIT_AND_EXPR))
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
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, GE_EXPR, BIT_IOR_EXPR))
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
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, GE_EXPR, BIT_IOR_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, LE_EXPR, GE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, LE_EXPR, GE_EXPR))
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
				if (gimple_simplify_356 (res_op, seq, valueize, type, captures, MAX_EXPR, LE_EXPR, BIT_AND_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, GE_EXPR, LE_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, GE_EXPR, LE_EXPR))
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
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MAX_EXPR, LE_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MAX_EXPR, LE_EXPR))
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
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MIN_EXPR, GE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MIN_EXPR, GE_EXPR))
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
				if (gimple_simplify_356 (res_op, seq, valueize, type, captures, MIN_EXPR, LE_EXPR, BIT_IOR_EXPR))
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
 && (TREE_CODE_CLASS (LE_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2441;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), LE_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2441;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), LE_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2441;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2441:;
					  }
				      }
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
 && (TREE_CODE_CLASS (LE_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2442;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2442;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2442;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2442:;
		      }
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
 && (TREE_CODE_CLASS (LE_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2443;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), LE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2443;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2443;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2443:;
		      }
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
      if (gimple_simplify_375 (res_op, seq, valueize, type, captures, LE_EXPR, LT_EXPR))
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
			      if (gimple_simplify_253 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR))
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
	if (gimple_simplify_255 (res_op, seq, valueize, type, captures, LE_EXPR))
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
				    if (gimple_simplify_257 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					if (gimple_simplify_257 (res_op, seq, valueize, type, captures, LE_EXPR))
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
		      if (gimple_simplify_258 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR))
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
					if (gimple_simplify_259 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					    if (gimple_simplify_259 (res_op, seq, valueize, type, captures, LE_EXPR))
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
			  if (gimple_simplify_258 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR))
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
	  if (gimple_simplify_260 (res_op, seq, valueize, type, captures, LE_EXPR))
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, PLUS_EXPR, LE_EXPR))
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
			      if (gimple_simplify_359 (res_op, seq, valueize, type, captures, LE_EXPR, GT_EXPR))
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, MINUS_EXPR, LE_EXPR))
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
				if (gimple_simplify_262 (res_op, seq, valueize, type, captures, LE_EXPR))
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, LE_EXPR, LE_EXPR))
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
			  if (gimple_simplify_266 (res_op, seq, valueize, type, captures, LE_EXPR, LE_EXPR))
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
				if (gimple_simplify_360 (res_op, seq, valueize, type, captures, LE_EXPR))
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
						if (gimple_simplify_361 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR))
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
				    if (gimple_simplify_270 (res_op, seq, valueize, type, captures, LE_EXPR))
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
		    if (gimple_simplify_271 (res_op, seq, valueize, type, captures, LE_EXPR))
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
				if (gimple_simplify_376 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR))
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, LE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, LE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, LE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, LE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, LE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, LE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_SQRT, LE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_SQRT, LE_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, GE_EXPR))
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
								    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LE_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LE_EXPR))
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
						    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LE_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LE_EXPR))
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
						if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LE_EXPR))
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
				if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LE_EXPR))
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
			if (gimple_simplify_362 (res_op, seq, valueize, type, captures, LE_EXPR))
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
						    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LE_EXPR))
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
				    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LE_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LE_EXPR))
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
				if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LE_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LE_EXPR))
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
		if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LE_EXPR))
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
	if (gimple_simplify_363 (res_op, seq, valueize, type, captures, LE_EXPR))
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
							      if (gimple_simplify_364 (res_op, seq, valueize, type, captures, LE_EXPR, GT_EXPR))
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
							      if (gimple_simplify_364 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
			      if (gimple_simplify_359 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
			if (gimple_simplify_365 (res_op, seq, valueize, type, captures, LE_EXPR))
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
					    if (gimple_simplify_377 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					      if (gimple_simplify_378 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR))
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
					      if (gimple_simplify_379 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR))
					        return true;
					    }
					  }
					if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q40 };
					      if (gimple_simplify_379 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR))
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
			if (gimple_simplify_380 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR))
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
				if (gimple_simplify_370 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR))
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
				if (gimple_simplify_371 (res_op, seq, valueize, type, captures, PLUS_EXPR, MINUS_EXPR, LE_EXPR, GE_EXPR))
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
				if (gimple_simplify_371 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR, LE_EXPR, GE_EXPR))
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
				if (gimple_simplify_296 (res_op, seq, valueize, type, captures, LE_EXPR, GE_EXPR))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, CFN_BUILT_IN_CTZ))
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
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_FFS))
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
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_FFSIMAX))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNT))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, CFN_BUILT_IN_CTZL))
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
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_FFSL))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNTLL))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, CFN_BUILT_IN_CTZIMAX))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, CFN_POPCOUNT))
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
			    if (gimple_simplify_373 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, CFN_CTZ))
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
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_FFS))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNTIMAX))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNTL))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LE_EXPR, NE_EXPR, CFN_BUILT_IN_CTZLL))
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
			    if (gimple_simplify_381 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_FFSLL))
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
gimple_simplify_CFN_BUILT_IN_POWIF (gimple_match_op *res_op, gimple_seq *seq,
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
		    case INTEGER_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			  if ((wi::to_wide (captures[1]) & 1) == 0
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2530;
			      {
				res_op->set_op (CFN_BUILT_IN_POWIF, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] = captures[1];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1127, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2530:;
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
		    case INTEGER_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			  if ((wi::to_wide (captures[1]) & 1) == 0
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2531;
			      {
				res_op->set_op (CFN_BUILT_IN_POWIF, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] = captures[1];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1127, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2531:;
			    }
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
	      case CFN_BUILT_IN_COPYSIGN:
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
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGN;
			    if ((wi::to_wide (captures[2]) & 1) == 0
)
			      {
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2532;
				{
				  res_op->set_op (CFN_BUILT_IN_POWIF, type, 2);
				  res_op->ops[0] = captures[0];
				  res_op->ops[1] = captures[2];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1128, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2532:;
			      }
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
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNF;
			    if ((wi::to_wide (captures[2]) & 1) == 0
)
			      {
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2533;
				{
				  res_op->set_op (CFN_BUILT_IN_POWIF, type, 2);
				  res_op->ops[0] = captures[0];
				  res_op->ops[1] = captures[2];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1128, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2533:;
			      }
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
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    const combined_fn copysigns = CFN_BUILT_IN_COPYSIGNL;
			    if ((wi::to_wide (captures[2]) & 1) == 0
)
			      {
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2534;
				{
				  res_op->set_op (CFN_BUILT_IN_POWIF, type, 2);
				  res_op->ops[0] = captures[0];
				  res_op->ops[1] = captures[2];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1128, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2534:;
			      }
			  }
		          break;
		        }
		      default:;
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
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    const combined_fn copysigns = CFN_COPYSIGN;
			    if ((wi::to_wide (captures[2]) & 1) == 0
)
			      {
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2535;
				{
				  res_op->set_op (CFN_BUILT_IN_POWIF, type, 2);
				  res_op->ops[0] = captures[0];
				  res_op->ops[1] = captures[2];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1128, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2535:;
			      }
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
if (real_onep (_p0))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_415 (res_op, seq, valueize, type, captures))
        return true;
    }
  }
  switch (TREE_CODE (_p1))
    {
    case INTEGER_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (wi::to_wide (captures[1]) == 0
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2536;
	      {
		tree tem;
		tem =  build_real (type, dconst1);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1129, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2536:;
	    }
	  else
	    {
	      if (wi::to_wide (captures[1]) == 1
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2537;
		  {
		    tree tem;
		    tem = captures[0];
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1130, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail2537:;
		}
	      else
		{
		  if (wi::to_wide (captures[1]) == -1
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2538;
		      {
			res_op->set_op (RDIV_EXPR, type, 2);
			res_op->ops[0] =  build_real (type, dconst1);
			res_op->ops[1] = captures[0];
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1131, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail2538:;
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
gimple_simplify_CFN_HYPOT (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn hypots = CFN_HYPOT;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2569;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1132, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2569:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn hypots = CFN_HYPOT;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2570;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1132, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2570:;
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
		    const combined_fn hypots = CFN_HYPOT;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2571;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1133, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2571:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		    const combined_fn hypots = CFN_HYPOT;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2572;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1133, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2572:;
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
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_HYPOT))
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
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_HYPOT))
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
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_HYPOT))
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
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_HYPOT))
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
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_HYPOT))
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
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_HYPOT))
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
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_HYPOT))
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
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_HYPOT))
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
      if (gimple_simplify_418 (res_op, seq, valueize, type, captures, CFN_HYPOT))
        return true;
    }
  }
  if (real_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
	if (gimple_simplify_418 (res_op, seq, valueize, type, captures, CFN_HYPOT))
	  return true;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_419 (res_op, seq, valueize, type, captures, CFN_HYPOT))
	  return true;
      }
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMINF32 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF32))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF32))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF32))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_506 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF32))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2612;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMAXF32, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2612;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2612:;
				      }
				  }
			          break;
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
gimple_simplify_CFN_BUILT_IN_FMINF64X (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF64X))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF64X))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF64X))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_506 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF64X))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2616;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMAXF64X, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2616;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2616:;
				      }
				  }
			          break;
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
gimple_simplify_CFN_BUILT_IN_FMAX (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAX))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAX))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAX))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_507 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAX))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2620;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMIN, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2620;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2620:;
				      }
				  }
			          break;
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
gimple_simplify_CFN_BUILT_IN_FMAXF64 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF64))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF64))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF64))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_507 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF64))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2624;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMINF64, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2624;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2624:;
				      }
				  }
			          break;
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
gimple_simplify_CFN_BUILT_IN_FMAXF128X (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF128X))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF128X))
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
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF128X))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_507 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF128X))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2628;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMINF128X, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2628;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2628:;
				      }
				  }
			          break;
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
gimple_simplify_UNORDERED_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
 && (TREE_CODE_CLASS (UNORDERED_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2632;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), UNORDERED_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2632;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), UNORDERED_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2632;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2632:;
					  }
				      }
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
 && (TREE_CODE_CLASS (UNORDERED_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2633;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNORDERED_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2633;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNORDERED_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2633;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2633:;
		      }
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
 && (TREE_CODE_CLASS (UNORDERED_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2634;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNORDERED_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2634;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNORDERED_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2634;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2634:;
		      }
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, UNORDERED_EXPR, UNORDERED_EXPR))
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, UNORDERED_EXPR, UNORDERED_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, UNORDERED_EXPR, UNORDERED_EXPR))
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
	  if (gimple_simplify_614 (res_op, seq, valueize, type, captures, UNORDERED_EXPR))
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
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2635;
	{
	  tree tem;
	  tem =  constant_boolean_node (true, type);
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1155, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail2635:;
      }
    else
      {
	if (!tree_expr_maybe_nan_p (captures[0]) && !tree_expr_maybe_nan_p (captures[1])
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2636;
	    {
	      tree tem;
	      tem =  constant_boolean_node (false, type);
	      res_op->set_value (tem);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1156, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2636:;
	  }
      }
  }
  return false;
}

bool
gimple_simplify_CFN_ADD_OVERFLOW (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_617 (res_op, seq, valueize, type, captures, CFN_ADD_OVERFLOW))
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
		    if (gimple_simplify_618 (res_op, seq, valueize, type, captures, CFN_ADD_OVERFLOW))
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
gimple_simplify_CFN_BUILT_IN_LDEXPF (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2661;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1160, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2661:;
	    }
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_SCALBNF (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2665;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1160, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2665:;
	    }
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_SCALBLNL (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2670;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1160, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2670:;
	    }
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify_BIT_FIELD_REF (gimple_match_op *res_op, gimple_seq *seq,
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
	      case BIT_FIELD_REF:
	        {
		  tree _q20 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q20) == SSA_NAME
		       || is_gimple_min_invariant (_q20)))
		    {
		      _q20 = do_valueize (valueize, _q20);
		      tree _q21 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 1);
		      if ((TREE_CODE (_q21) == SSA_NAME
		           || is_gimple_min_invariant (_q21)))
		        {
			  _q21 = do_valueize (valueize, _q21);
			  tree _q22 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 2);
			  if ((TREE_CODE (_q22) == SSA_NAME
			       || is_gimple_min_invariant (_q22)))
			    {
			      _q22 = do_valueize (valueize, _q22);
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _p1, _p2 };
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2869;
				{
				  res_op->set_op (BIT_FIELD_REF, type, 3);
				  res_op->ops[0] = captures[0];
				  res_op->ops[1] = captures[3];
				  res_op->ops[2] =  const_binop (PLUS_EXPR, bitsizetype, captures[2], captures[4]);
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1203, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2869:;
			      }
			    }
		        }
		    }
	          break;
	        }
	      case VIEW_CONVERT_EXPR:
	        {
		  tree _q20 = TREE_OPERAND (gimple_assign_rhs1 (_a1), 0);
		  if ((TREE_CODE (_q20) == SSA_NAME
		       || is_gimple_min_invariant (_q20)))
		    {
		      _q20 = do_valueize (valueize, _q20);
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			if (! INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 || type_has_mode_precision_p (TREE_TYPE (captures[0]))
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2870;
			    {
			      res_op->set_op (BIT_FIELD_REF, type, 3);
			      res_op->ops[0] = captures[0];
			      res_op->ops[1] = captures[1];
			      res_op->ops[2] = captures[2];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1204, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2870:;
			  }
		      }
		    }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  if (integer_zerop (_p2))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (tree_int_cst_equal (captures[1], TYPE_SIZE (TREE_TYPE (captures[0])))
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2871;
	    {
	      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	      res_op->ops[0] = captures[0];
	      res_op->resimplify (lseq, valueize);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1205, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2871:;
	  }
      }
    }
  {
    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _p2 };
    if (TREE_CODE (TREE_TYPE (captures[0])) == COMPLEX_TYPE
 && tree_int_cst_equal (captures[1], TYPE_SIZE (TREE_TYPE (TREE_TYPE (captures[0]))))
)
      {
	if (integer_zerop (captures[2])
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2872;
	    {
	      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	      {
		tree _o1[1], _r1;
		_o1[0] = captures[0];
		gimple_match_op tem_op (res_op->cond.any_else (), REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o1[0])), _o1[0]);
		tem_op.resimplify (lseq, valueize);
		_r1 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r1) goto next_after_fail2872;
		res_op->ops[0] = _r1;
	      }
	      res_op->resimplify (lseq, valueize);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1206, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2872:;
	  }
	else
	  {
	    if (tree_int_cst_equal (captures[2], TYPE_SIZE (TREE_TYPE (TREE_TYPE (captures[0]))))
)
	      {
		gimple_seq *lseq = seq;
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2873;
		{
		  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
		  {
		    tree _o1[1], _r1;
		    _o1[0] = captures[0];
		    gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o1[0])), _o1[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r1) goto next_after_fail2873;
		    res_op->ops[0] = _r1;
		  }
		  res_op->resimplify (lseq, valueize);
		  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1207, __FILE__, __LINE__, true);
		  return true;
		}
next_after_fail2873:;
	      }
	  }
      }
    else
      {
	if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (type)
 && (!
1
 || is_gimple_reg (captures[0]))
 && ((compare_tree_int (captures[1], TYPE_PRECISION (TREE_TYPE (captures[0]))) == 0
 && integer_zerop (captures[2]))
 || (BYTES_BIG_ENDIAN == WORDS_BIG_ENDIAN
 && canonicalize_math_after_vectorization_p ()
 && TYPE_PRECISION (TREE_TYPE (captures[0])) % BITS_PER_UNIT == 0
 && TYPE_PRECISION (type) % BITS_PER_UNIT == 0
 && compare_tree_int (captures[2], (BYTES_BIG_ENDIAN
 ? (TYPE_PRECISION (TREE_TYPE (captures[0]))
 - TYPE_PRECISION (type))
 : 0)) == 0))
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2874;
	    {
	      res_op->set_op (NOP_EXPR, type, 1);
	      res_op->ops[0] = captures[0];
	      res_op->resimplify (lseq, valueize);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1208, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2874:;
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
	      case CONSTRUCTOR:
	        {
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _p2 };
		    if (VECTOR_TYPE_P (TREE_TYPE (captures[0]))
 && tree_fits_uhwi_p (TYPE_SIZE (type))
 && ((tree_to_uhwi (TYPE_SIZE (type))
 == tree_to_uhwi (TYPE_SIZE (TREE_TYPE (TREE_TYPE (captures[0])))))
 || (VECTOR_TYPE_P (type)
 && (tree_to_uhwi (TYPE_SIZE (TREE_TYPE (type)))
 == tree_to_uhwi (TYPE_SIZE (TREE_TYPE (TREE_TYPE (captures[0])))))))
)
		      {
			{
 tree ctor = (TREE_CODE (captures[0]) == SSA_NAME
 ? gimple_assign_rhs1 (SSA_NAME_DEF_STMT (captures[0])) : captures[0]);
 tree eltype = TREE_TYPE (TREE_TYPE (ctor));
 unsigned HOST_WIDE_INT width = tree_to_uhwi (TYPE_SIZE (eltype));
 unsigned HOST_WIDE_INT n = tree_to_uhwi (captures[1]);
 unsigned HOST_WIDE_INT idx = tree_to_uhwi (captures[2]);
			    if (n != 0
 && (idx % width) == 0
 && (n % width) == 0
 && known_le ((idx + n) / width,
 TYPE_VECTOR_SUBPARTS (TREE_TYPE (ctor)))
)
			      {
				{
 idx = idx / width;
 n = n / width;
 poly_uint64 k = 1;
 if (CONSTRUCTOR_NELTS (ctor) != 0)
 {
 tree cons_elem = TREE_TYPE (CONSTRUCTOR_ELT (ctor, 0)->value);
 if (TREE_CODE (cons_elem) == VECTOR_TYPE)
 k = TYPE_VECTOR_SUBPARTS (cons_elem);
 }
 unsigned HOST_WIDE_INT elt, count, const_k;
				    if (multiple_p (idx, k, &elt) && multiple_p (n, k, &count)
)
				      {
					if (CONSTRUCTOR_NELTS (ctor) == 0
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2875;
					    {
					      tree tem;
					      tem =  build_zero_cst (type);
					      res_op->set_value (tem);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1209, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2875:;
					  }
					else
					  {
					    if (count == 1
)
					      {
						if (elt < CONSTRUCTOR_NELTS (ctor)
)
						  {
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2876;
						    {
						      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						      res_op->ops[0] =  CONSTRUCTOR_ELT (ctor, elt)->value;
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1210, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2876:;
						  }
						else
						  {
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2877;
						    {
						      tree tem;
						      tem =  build_zero_cst (type);
						      res_op->set_value (tem);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1211, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2877:;
						  }
					      }
					    else
					      {
						if (single_use (captures[0])
)
						  {
						    {
 vec<constructor_elt, va_gc> *vals;
 vec_alloc (vals, count);
 bool constant_p = true;
 tree res;
 for (unsigned i = 0;
 i < count && elt + i < CONSTRUCTOR_NELTS (ctor); ++i)
 {
 tree e = CONSTRUCTOR_ELT (ctor, elt + i)->value;
 CONSTRUCTOR_APPEND_ELT (vals, NULL_TREE, e);
 if (!CONSTANT_CLASS_P (e))
 constant_p = false;
 }
 tree evtype = (types_match (TREE_TYPE (type),
 TREE_TYPE (TREE_TYPE (ctor)))
 ? type
 : build_vector_type (TREE_TYPE (TREE_TYPE (ctor)),
 count * k));
 res = (constant_p ? build_vector_from_ctor (evtype, vals)
 : (
1
 ? NULL_TREE : build_constructor (evtype, vals)));
							if (res
)
							  {
							    gimple_seq *lseq = seq;
							    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2878;
							    {
							      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
							      res_op->ops[0] =  res;
							      res_op->resimplify (lseq, valueize);
							      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1212, __FILE__, __LINE__, true);
							      return true;
							    }
next_after_fail2878:;
							  }
						    }
						  }
					      }
					  }
				      }
				    else
				      {
					if (k.is_constant (&const_k)
 && idx + n <= (idx / const_k + 1) * const_k
)
					  {
					    if (CONSTRUCTOR_NELTS (ctor) <= idx / const_k
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2879;
						{
						  tree tem;
						  tem =  build_zero_cst (type);
						  res_op->set_value (tem);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1213, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2879:;
					      }
					    else
					      {
						if (n == const_k
)
						  {
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2880;
						    {
						      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						      res_op->ops[0] =  CONSTRUCTOR_ELT (ctor, idx / const_k)->value;
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1214, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2880:;
						  }
						else
						  {
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2881;
						    {
						      res_op->set_op (BIT_FIELD_REF, type, 3);
						      res_op->ops[0] =  CONSTRUCTOR_ELT (ctor, idx / const_k)->value;
						      res_op->ops[1] = captures[1];
						      res_op->ops[2] =  bitsize_int ((idx % const_k) * width);
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1215, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2881:;
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
	      case BIT_INSERT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  tree _q22 = gimple_assign_rhs3 (_a1);
		  _q22 = do_valueize (valueize, _q22);
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _p1, _p2 };
		    {
 unsigned HOST_WIDE_INT isize;
 if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1])))
 isize = TYPE_PRECISION (TREE_TYPE (captures[1]));
 else
 isize = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (captures[1])));
			if ((!INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 || type_has_mode_precision_p (TREE_TYPE (captures[1])))
 && wi::leu_p (wi::to_wide (captures[2]), wi::to_wide (captures[4]))
 && wi::leu_p (wi::to_wide (captures[4]) + wi::to_wide (captures[3]),
 wi::to_wide (captures[2]) + isize)
)
			  {
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2882;
			    {
			      res_op->set_op (BIT_FIELD_REF, type, 3);
			      res_op->ops[0] = captures[1];
			      res_op->ops[1] = captures[3];
			      res_op->ops[2] =  wide_int_to_tree (bitsizetype,
 wi::to_wide (captures[4])
 - wi::to_wide (captures[2]));
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1216, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail2882:;
			  }
			else
			  {
			    if (wi::eq_p (wi::to_wide (captures[2]), wi::to_wide (captures[4]))
 && compare_tree_int (captures[3], isize) == 0
)
			      {
				gimple_seq *lseq = seq;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2883;
				{
				  res_op->set_op (NOP_EXPR, type, 1);
				  res_op->ops[0] = captures[1];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1217, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail2883:;
			      }
			    else
			      {
				if (wi::geu_p (wi::to_wide (captures[2]),
 wi::to_wide (captures[4]) + wi::to_wide (captures[3]))
 || wi::geu_p (wi::to_wide (captures[4]),
 wi::to_wide (captures[2]) + isize)
)
				  {
				    gimple_seq *lseq = seq;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2884;
				    {
				      res_op->set_op (BIT_FIELD_REF, type, 3);
				      res_op->ops[0] = captures[0];
				      res_op->ops[1] = captures[3];
				      res_op->ops[2] = captures[4];
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1218, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail2884:;
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
gimple_simplify_CFN_FMA (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_641 (res_op, seq, valueize, type, captures, CFN_FMA))
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
				    if (gimple_simplify_642 (res_op, seq, valueize, type, captures, CFN_FMA))
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
		    if (gimple_simplify_641 (res_op, seq, valueize, type, captures, CFN_FMA))
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
		    if (gimple_simplify_643 (res_op, seq, valueize, type, captures, CFN_FMA))
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
				    if (gimple_simplify_642 (res_op, seq, valueize, type, captures, CFN_FMA))
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
gimple_simplify_CFN_FNMS (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_648 (res_op, seq, valueize, type, captures))
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
				    if (gimple_simplify_649 (res_op, seq, valueize, type, captures))
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
		    if (gimple_simplify_648 (res_op, seq, valueize, type, captures))
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2888;
			{
			  res_op->set_op (CFN_FNMA, type, 3);
			  res_op->ops[0] = captures[0];
			  res_op->ops[1] = captures[1];
			  res_op->ops[2] = captures[2];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1222, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2888:;
		      }
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
				    if (gimple_simplify_649 (res_op, seq, valueize, type, captures))
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
gimple_simplify (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper code, const tree type, tree _p0, tree _p1, tree _p2)
{
  switch (code.get_rep())
    {
    case COND_EXPR:
      return gimple_simplify_COND_EXPR (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case VEC_COND_EXPR:
      return gimple_simplify_VEC_COND_EXPR (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case BIT_FIELD_REF:
      return gimple_simplify_BIT_FIELD_REF (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case BIT_INSERT_EXPR:
      return gimple_simplify_BIT_INSERT_EXPR (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case -CFN_BUILT_IN_FMAF:
      return gimple_simplify_CFN_BUILT_IN_FMAF (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case -CFN_BUILT_IN_FMA:
      return gimple_simplify_CFN_BUILT_IN_FMA (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case -CFN_BUILT_IN_FMAL:
      return gimple_simplify_CFN_BUILT_IN_FMAL (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case -CFN_FMA:
      return gimple_simplify_CFN_FMA (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case -CFN_FMS:
      return gimple_simplify_CFN_FMS (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case -CFN_FNMA:
      return gimple_simplify_CFN_FNMA (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case -CFN_FNMS:
      return gimple_simplify_CFN_FNMS (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case -CFN_VCOND_MASK_LEN:
      return gimple_simplify_CFN_VCOND_MASK_LEN (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    case VEC_PERM_EXPR:
      return gimple_simplify_VEC_PERM_EXPR (res_op, seq, valueize, code, type, _p0, _p1, _p2);
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_COND_MUL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2, tree _p3)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p2))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p2))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q40 = gimple_call_arg (_c1, 0);
		    _q40 = do_valueize (valueize, _q40);
		    tree _q41 = gimple_call_arg (_c1, 1);
		    _q41 = do_valueize (valueize, _q41);
		    if (real_onep (_q40))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q41, _p3 };
			  {
 tree signs = sign_mask_for (type);
			      if (signs
)
				{
				  {
 tree inttype = TREE_TYPE (signs);
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2902;
				      {
					res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
					{
					  tree _o1[4], _r1;
					  _o1[0] = captures[0];
					  {
					    tree _o2[1], _r2;
					    _o2[0] = captures[1];
					    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, inttype, _o2[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail2902;
					    _o1[1] = _r2;
					  }
					  {
					    tree _o2[2], _r2;
					    {
					      tree _o3[1], _r3;
					      _o3[0] = captures[2];
					      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, inttype, _o3[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r3) goto next_after_fail2902;
					      _o2[0] = _r3;
					    }
					    _o2[1] =  signs;
					    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail2902;
					    _o1[2] = _r2;
					  }
					  {
					    tree _o2[1], _r2;
					    _o2[0] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, inttype, _o2[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail2902;
					    _o1[3] = _r2;
					  }
					  gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_XOR, inttype, _o1[0], _o1[1], _o1[2], _o1[3]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail2902;
					  res_op->ops[0] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1235, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2902:;
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    if (real_onep (_q30))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p2, _q31, _p3 };
			  {
 tree signs = sign_mask_for (type);
			      if (signs
)
				{
				  {
 tree inttype = TREE_TYPE (signs);
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2903;
				      {
					res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
					{
					  tree _o1[4], _r1;
					  _o1[0] = captures[0];
					  {
					    tree _o2[1], _r2;
					    _o2[0] = captures[1];
					    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, inttype, _o2[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail2903;
					    _o1[1] = _r2;
					  }
					  {
					    tree _o2[2], _r2;
					    {
					      tree _o3[1], _r3;
					      _o3[0] = captures[2];
					      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, inttype, _o3[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r3) goto next_after_fail2903;
					      _o2[0] = _r3;
					    }
					    _o2[1] =  signs;
					    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail2903;
					    _o1[2] = _r2;
					  }
					  {
					    tree _o2[1], _r2;
					    _o2[0] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, inttype, _o2[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail2903;
					    _o1[3] = _r2;
					  }
					  gimple_match_op tem_op (res_op->cond.any_else (), CFN_COND_XOR, inttype, _o1[0], _o1[1], _o1[2], _o1[3]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail2903;
					  res_op->ops[0] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1235, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2903:;
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
  return false;
}

bool
gimple_simplify (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper code, const tree type, tree _p0, tree _p1, tree _p2, tree _p3, tree _p4)
{
  switch (code.get_rep())
    {
    case -CFN_VCOND_MASK_LEN:
      return gimple_simplify_CFN_VCOND_MASK_LEN (res_op, seq, valueize, code, type, _p0, _p1, _p2, _p3, _p4);
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_COND_LEN_ADD (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2, tree _p3, tree _p4, tree _p5)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_truep (_p0))
    {
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
				tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _q42, _p4, _p5 };
				if (ANY_INTEGRAL_TYPE_P (type)
 || (FLOAT_TYPE_P (type)
 && fold_real_zero_addition_p (type, NULL_TREE, captures[3], 0))
)
				  {
				    gimple_seq *lseq = seq;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2977;
				    {
				      res_op->set_op (CFN_COND_LEN_ADD, type, 6);
				      res_op->ops[0] = captures[1];
				      res_op->ops[1] = captures[0];
				      res_op->ops[2] = captures[2];
				      res_op->ops[3] = captures[0];
				      res_op->ops[4] = captures[4];
				      res_op->ops[5] = captures[5];
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1241, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail2977:;
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
  return false;
}

bool
gimple_simplify (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper code, const tree type, tree _p0, tree _p1, tree _p2, tree _p3, tree _p4, tree _p5)
{
  switch (code.get_rep())
    {
    case -CFN_COND_LEN_ADD:
      return gimple_simplify_CFN_COND_LEN_ADD (res_op, seq, valueize, code, type, _p0, _p1, _p2, _p3, _p4, _p5);
    default:;
    }
  return false;
}

bool
gimple_simplify (gimple_match_op*, gimple_seq*,
                 tree (*)(tree), code_helper,
                 const tree, tree, tree, tree, tree, tree, tree, tree)
{
  return false;
}
void
gimple_dump_logs (const char *file1, int line1_id, const char *file2, int line2, bool simplify)
{
  static int dbg_line_numbers[1242] = {
	160, 163, 172, 174, 177, 180, 182, 188, 196, 194, 198, 200, 204, 921, 919, 2041, 2052, 2033, 2039, 2044, 
	2049, 2265, 2269, 2271, 2273, 2275, 2277, 2279, 2281, 2308, 2324, 2332, 2314, 2871, 2873, 2878, 2876, 2885, 2883, 3065, 
	3075, 3087, 3094, 3101, 3108, 3115, 3122, 3135, 3141, 3146, 3151, 3156, 3162, 3194, 3168, 3173, 3178, 3205, 3219, 3233, 
	3261, 3247, 3274, 3281, 3315, 3322, 3331, 3288, 3295, 3308, 3301, 3350, 3368, 3382, 3396, 3409, 3422, 3464, 3449, 3482, 
	5521, 5764, 5775, 5753, 5782, 5814, 5789, 5801, 5821, 8645, 8666, 8690, 10760, 10763, 10768, 10983, 10986, 11007, 11012, 8478, 
	227, 1283, 1287, 1413, 1428, 1433, 1528, 1533, 1626, 1750, 1840, 1760, 1904, 1770, 1775, 1898, 1783, 1950, 1892, 1911, 
	1917, 2510, 1924, 1938, 1984, 2179, 2246, 2257, 2260, 2292, 2387, 2419, 4475, 4488, 4498, 2934, 2936, 4512, 4524, 4752, 
	6740, 6742, 6791, 6799, 9834, 9840, 10236, 10238, 253, 280, 281, 475, 1277, 4439, 1709, 1714, 1733, 1788, 1851, 3989, 
	3927, 4032, 4033, 4037, 4038, 4048, 4052, 4056, 4071, 4074, 4078, 4004, 4007, 4012, 4145, 4159, 4165, 4171, 4353, 4354, 
	4380, 4399, 4418, 4740, 5438, 6671, 8159, 8219, 9689, 9737, 9751, 10787, 11100, 903, 909, 1393, 1402, 1869, 1860, 1807, 
	2536, 3940, 3980, 3985, 3986, 3994, 3995, 3998, 4015, 4090, 4093, 4097, 4179, 4201, 4239, 4250, 4289, 4304, 4746, 6681, 
	8224, 9747, 10791, 1379, 1388, 1418, 1423, 1465, 1450, 1457, 1491, 1946, 1496, 1503, 1523, 1740, 1650, 1745, 1755, 1765, 
	1879, 1966, 1976, 1978, 2198, 2206, 2211, 2216, 2232, 2222, 2223, 2370, 2399, 2401, 2406, 2408, 2529, 3548, 3563, 3581, 
	3764, 3765, 3766, 3771, 3776, 3782, 3788, 3820, 3821, 3826, 3827, 3832, 3837, 3841, 3890, 3904, 3553, 3572, 3574, 3558, 
	7325, 9042, 429, 437, 443, 449, 481, 1222, 1242, 1247, 2025, 2026, 2063, 2545, 5431, 5465, 5467, 5516, 8152, 8513, 
	8507, 8552, 8557, 311, 866, 2297, 2524, 2563, 2573, 2612, 2618, 2621, 2626, 2791, 2642, 2644, 2645, 2648, 2650, 2651, 
	2653, 2655, 2657, 2659, 2661, 2663, 2677, 2678, 2712, 2797, 2822, 2738, 2765, 2805, 2810, 2835, 2842, 2893, 3862, 4692, 
	4700, 4703, 4709, 4712, 4978, 4979, 5019, 5023, 5152, 4986, 4987, 5037, 5040, 5041, 5046, 5049, 5052, 5053, 4993, 5005, 
	5008, 5197, 5201, 5206, 5449, 5581, 5585, 6751, 6767, 6769, 6781, 6811, 6817, 6818, 6833, 6834, 6838, 6856, 6861, 6890, 
	6898, 6908, 6917, 6921, 6927, 6934, 6950, 6954, 6958, 6965, 6966, 6971, 6972, 6979, 6980, 6982, 6983, 6999, 7010, 7022, 
	7026, 7028, 7033, 7036, 7039, 7047, 7109, 7110, 7114, 7115, 7122, 7126, 7130, 7133, 7139, 7140, 7144, 7147, 7154, 7174, 
	7177, 7180, 7183, 7232, 7235, 7239, 7246, 7252, 7260, 7269, 7270, 7368, 7378, 7417, 7423, 7439, 7452, 7454, 7456, 7482, 
	7524, 7503, 7504, 7511, 7513, 7529, 7535, 7540, 7549, 7555, 7561, 7566, 7571, 7603, 7585, 7595, 7616, 7680, 7718, 7720, 
	7722, 7724, 7726, 7728, 7732, 7734, 7757, 7780, 7892, 7899, 7900, 7908, 7909, 8072, 9086, 9088, 9176, 9178, 9179, 9185, 
	9201, 9203, 9205, 9223, 9224, 9493, 9496, 9510, 9519, 9570, 9571, 9625, 9627, 9697, 10106, 10108, 10110, 10542, 6747, 6758, 
	6760, 6868, 455, 1195, 1199, 1216, 8463, 8468, 8473, 514, 515, 526, 531, 538, 547, 555, 564, 610, 613, 801, 
	970, 977, 987, 992, 1001, 1009, 1034, 1055, 575, 831, 835, 840, 846, 850, 858, 946, 726, 736, 738, 740, 
	8146, 8518, 8540, 8523, 8528, 8533, 8547, 8568, 8573, 788, 791, 1601, 2586, 2589, 2686, 2698, 2780, 2724, 2751, 2863, 
	2866, 4730, 4722, 6705, 8002, 7282, 7285, 7311, 7312, 7805, 7807, 7809, 7811, 7815, 7817, 7819, 7821, 7825, 7830, 7837, 
	7842, 7862, 7864, 7866, 7868, 8015, 8023, 8033, 8041, 8051, 8173, 8175, 9100, 9119, 9159, 9555, 9557, 9558, 9606, 9609, 
	9611, 2602, 6695, 7355, 10127, 10129, 10131, 10133, 10137, 892, 928, 1267, 4858, 4902, 4905, 4913, 4927, 4953, 4955, 4956, 
	4957, 5164, 4883, 4892, 5635, 5638, 5639, 5647, 10062, 10080, 2190, 3873, 3878, 4638, 8716, 8730, 8746, 8757, 8925, 8927, 
	8958, 8993, 11044, 1159, 8411, 8432, 1168, 8338, 8339, 8458, 8872, 1182, 1186, 8453, 8623, 2070, 2108, 2131, 2132, 2474, 
	3965, 9440, 1361, 1445, 1477, 1438, 1470, 1485, 1518, 1559, 1570, 1581, 1593, 2431, 1665, 1822, 1830, 1835, 1958, 2287, 
	2551, 3061, 5123, 5141, 5142, 5606, 5612, 5660, 3518, 3520, 3530, 3532, 3637, 3638, 3639, 3644, 3649, 3655, 3661, 3693, 
	3694, 3699, 3700, 3705, 3710, 3714, 3538, 3543, 3588, 6003, 6012, 6013, 6883, 7328, 9028, 9030, 9732, 1508, 1513, 2489, 
	1681, 2494, 5175, 2469, 2500, 2504, 5189, 4681, 6732, 6734, 4531, 4535, 4545, 4563, 4621, 4623, 4625, 4627, 4676, 9050, 
	4549, 4556, 4541, 4648, 4657, 4660, 4758, 4770, 4789, 4796, 4802, 4808, 4814, 4782, 4825, 4838, 5993, 6146, 6148, 6150, 
	6250, 6253, 6277, 6278, 6281, 6282, 6286, 6287, 6291, 6292, 6296, 6297, 6301, 6302, 6399, 6401, 6415, 6419, 6092, 6093, 
	6360, 6364, 6372, 6379, 6384, 6452, 6458, 6467, 6468, 6477, 6483, 6490, 6546, 6551, 6612, 6615, 6559, 6564, 6622, 6625, 
	7663, 7665, 6581, 6582, 6583, 6633, 7633, 7634, 7648, 7649, 6602, 6603, 6604, 6645, 6646, 7694, 7699, 9059, 9060, 9894, 
	9902, 9930, 9942, 9969, 9981, 4845, 8101, 8104, 8123, 5557, 5569, 5572, 5576, 5873, 6314, 6323, 10301, 10308, 10336, 10342, 
	10350, 10356, 10365, 10371, 10379, 10385, 10431, 10439, 7924, 6876, 6872, 8082, 8089, 8209, 8214, 8302, 8374, 8391, 8423, 8443, 
	8448, 8686, 8663, 8642, 8484, 8488, 8766, 8776, 8783, 8792, 8810, 8817, 8798, 9065, 9071, 9430, 9436, 9433, 9444, 9450, 
	9458, 9464, 9472, 9478, 9528, 9671, 9673, 9675, 9712, 9727, 9790, 9792, 9794, 9993, 9996, 9801, 9815, 9828, 9883, 9885, 
	9887, 10094, 10181, 10183, 10185, 10319, 10326, 10512, 10526, 10617, 10618, 218, 1990, 2000, 5692, 2009, 1109, 2442, 3042, 5217, 
	5219, 5229, 5321, 5332, 5345, 5368, 5378, 5387, 5399, 5425, 5700, 6100, 1116, 1210, 2079, 2083, 2114, 2120, 3952, 6658, 
	3960, 9454, 9468, 9482, 1228, 1235, 8437, 8618, 1678, 1692, 1699, 1721, 1728, 2302, 2517, 1995, 2004, 2013, 5234, 5243, 
	5255, 5265, 5271, 5709, 5412, 5280, 5277, 5529, 5537, 5545, 5548, 5532, 5540, 5551, 5563, 8295, 8270, 8231, 8235, 8581, 
	8359, 8597, 8585, 8589, 8363, 8601, 8593, 8632, 8823, 8830, 8836, 10533, 236, 1634, 1639, 2355, 2482, 3853, 5728, 5740, 
	5750, 11059, 708, 4130, 4458, 5478, 5484, 5486, 2941, 241, 2962, 2966, 2978, 259, 296, 1397, 1687, 1795, 1802, 1812, 
	1817, 2089, 2986, 2998, 2992, 3004, 2100, 2707, 2732, 2759, 2743, 2770, 4213, 4185, 4260, 4269, 4312, 4323, 5491, 5502, 
	11025, 11026, 1606, 1611, 1672, 7318, 10490, 10494, 246, 299, 2716, 3011, 3018, 3025, 3031, 4222, 3036, 4278, 4326, 4335, 
	5494, 355, 303, 323, 332, 340, 414, 627, 667, 962, 1147, 1152, 1250, 1253, 1353, 2345, 5507, 8181, 8187, 8350, 
	8562, 2462, 5907, 5915, 7385, 7956, 7982, 7983, 2452, 7950, 1142, 953, 1086, 1091, 590, 593, 717, 746, 752, 758, 
	766, 771, 776, 819, 823, 8193, 8199, 7874, 877, 1308, 4874, 1297, 1300, 1332, 1343, 4851, 4897, 1123, 8605, 8367, 
	8613, 8609, 8850, 8853, 8856, 8861, 8867, 1129, 1175, 8879, 8882, 8885, 1134, 1137, 1367, 1540, 1616, 1644, 1658, 2237, 
	4964, 4965, 7322, 11032, 4570, 4574, 4602, 4603, 4669, 4581, 4585, 4593, 4594, 4934, 5526, 7932, 7934, 7941, 7943, 6880, 
	8493, 5678, 5680, 5941, 5946, 5952, 5953, 5959, 5964, 5969, 5975, 5977, 5984, 6431, 6434, 6437, 6440, 6496, 6499, 6508, 
	6512, 6517, 6525, 7747, 7748, 5840, 5844, 5848, 5852, 5684, 5686, 5880, 5884, 5888, 5892, 5926, 5928, 6337, 6351, 10206, 
	10212, 10266, 10273, 9231, 9237, 9242, 9251, 9253, 9273, 9318, 9321, 9322, 9354, 9360, 9362, 9363, 9385, 9390, 9395, 9423, 
	9447, 9461, 9475, 10641, 10643, 10678, 10752, 10755, 10840, 10775, 10781, 10909, 10911, 10973, 10975, 465, 10400, 10221, 10227, 10283, 
	10290, 10415
  };

  fprintf (dump_file, "%s %s:%d, %s:%d\n",
	  simplify ? "Applying pattern" : "Matching expression", file1, dbg_line_numbers[line1_id], file2, line2);
}

#pragma GCC diagnostic pop
