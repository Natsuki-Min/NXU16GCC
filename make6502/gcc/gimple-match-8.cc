/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "gimple-match-auto.h"

bool
gimple_bit_xor_cst (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
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
	      case BIT_XOR_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  if (uniform_integer_cst_p (_p1))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
			{
			  res_ops[0] = captures[0];
			  res_ops[1] = captures[1];
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 4, __FILE__, __LINE__, false);
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
gimple_with_possible_nonzero_bits (tree t, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
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
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 33, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case SSA_NAME:
      {
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { t };
	  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
	    {
	      {
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 34, __FILE__, __LINE__, false);
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
gimple_usadd_left_part_1 (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
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
	      case PLUS_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
		      {
			{
			  res_ops[0] = captures[0];
			  res_ops[1] = captures[1];
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 41, __FILE__, __LINE__, false);
			  return true;
			}
		      }
		  }
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
		    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
		      {
			{
			  res_ops[0] = captures[0];
			  res_ops[1] = captures[1];
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 41, __FILE__, __LINE__, false);
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
gimple_unsigned_integer_sat_trunc (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
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
		  switch (TREE_CODE (_p0))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p0))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
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
					      CASE_CONVERT:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  switch (TREE_CODE (_q30))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q30))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case GT_EXPR:
							        {
								  tree _q40 = gimple_assign_rhs1 (_a4);
								  _q40 = do_valueize (valueize, _q40);
								  tree _q41 = gimple_assign_rhs2 (_a4);
								  _q41 = do_valueize (valueize, _q41);
								  switch (TREE_CODE (_q41))
								    {
								    case INTEGER_CST:
								      {
									switch (TREE_CODE (_p1))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _p1))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    CASE_CONVERT:
										      {
											tree _q70 = gimple_assign_rhs1 (_a5);
											_q70 = do_valueize (valueize, _q70);
											if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
											  {
											    {
											      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q41 };
											      if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
												{
												  {
 unsigned itype_precision = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_precision = TYPE_PRECISION (type);
 wide_int trunc_max = wi::mask (otype_precision, false, itype_precision);
 wide_int int_cst = wi::to_wide (captures[1], itype_precision);
												      if (otype_precision < itype_precision && wi::eq_p (trunc_max, int_cst)
)
													{
													  {
													    res_ops[0] = captures[0];
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 76, __FILE__, __LINE__, false);
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
							          break;
							        }
							      default:;
							      }
						        }
						      break;
						    default:;
						    }
					          break;
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
				  switch (TREE_CODE (_p1))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _p1))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case NEGATE_EXPR:
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
							      CASE_CONVERT:
							        {
								  tree _q50 = gimple_assign_rhs1 (_a4);
								  _q50 = do_valueize (valueize, _q50);
								  switch (TREE_CODE (_q50))
								    {
								    case SSA_NAME:
								      if (gimple *_d5 = get_def (valueize, _q50))
								        {
									  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									    switch (gimple_assign_rhs_code (_a5))
									      {
									      case GT_EXPR:
									        {
										  tree _q60 = gimple_assign_rhs1 (_a5);
										  _q60 = do_valueize (valueize, _q60);
										  tree _q61 = gimple_assign_rhs2 (_a5);
										  _q61 = do_valueize (valueize, _q61);
										  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
										    {
										      switch (TREE_CODE (_q61))
										        {
											case INTEGER_CST:
											  {
											    {
											      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q61 };
											      if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
												{
												  {
 unsigned itype_precision = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_precision = TYPE_PRECISION (type);
 wide_int trunc_max = wi::mask (otype_precision, false, itype_precision);
 wide_int int_cst = wi::to_wide (captures[1], itype_precision);
												      if (otype_precision < itype_precision && wi::eq_p (trunc_max, int_cst)
)
													{
													  {
													    res_ops[0] = captures[0];
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 76, __FILE__, __LINE__, false);
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
								        }
								      break;
								    default:;
								    }
							          break;
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
						  tree _q40 = gimple_assign_rhs1 (_a3);
						  _q40 = do_valueize (valueize, _q40);
						  tree _q41 = gimple_assign_rhs2 (_a3);
						  _q41 = do_valueize (valueize, _q41);
						  if (tree_swap_operands_p (_q40, _q41))
						    std::swap (_q40, _q41);
						  switch (TREE_CODE (_q40))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q40))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      CASE_CONVERT:
							        {
								  tree _q50 = gimple_assign_rhs1 (_a4);
								  _q50 = do_valueize (valueize, _q50);
								  switch (TREE_CODE (_q50))
								    {
								    case SSA_NAME:
								      if (gimple *_d5 = get_def (valueize, _q50))
								        {
									  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									    switch (gimple_assign_rhs_code (_a5))
									      {
									      case LE_EXPR:
									        {
										  tree _q60 = gimple_assign_rhs1 (_a5);
										  _q60 = do_valueize (valueize, _q60);
										  tree _q61 = gimple_assign_rhs2 (_a5);
										  _q61 = do_valueize (valueize, _q61);
										  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
										    {
										      switch (TREE_CODE (_q61))
										        {
											case INTEGER_CST:
											  {
											    switch (TREE_CODE (_q41))
											      {
											      case INTEGER_CST:
											        {
												  {
												    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q61, _q41 };
												    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
												      {
													{
 unsigned itype_precision = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_precision = TYPE_PRECISION (type);
 wide_int trunc_max = wi::mask (otype_precision, false, itype_precision);
 wide_int max = wi::mask (otype_precision, false, otype_precision);
 wide_int int_cst_1 = wi::to_wide (captures[1]);
 wide_int int_cst_2 = wi::to_wide (captures[2]);
													    if (wi::eq_p (trunc_max, int_cst_1) && wi::eq_p (max, int_cst_2)
)
													      {
														{
														  res_ops[0] = captures[0];
														  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 77, __FILE__, __LINE__, false);
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
									        }
									      default:;
									      }
								        }
								      break;
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
									tree _q60 = gimple_assign_rhs1 (_a4);
									_q60 = do_valueize (valueize, _q60);
									switch (TREE_CODE (_q60))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q60))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case LE_EXPR:
										      {
											tree _q70 = gimple_assign_rhs1 (_a5);
											_q70 = do_valueize (valueize, _q70);
											tree _q71 = gimple_assign_rhs2 (_a5);
											_q71 = do_valueize (valueize, _q71);
											if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
											  {
											    switch (TREE_CODE (_q71))
											      {
											      case INTEGER_CST:
											        {
												  {
												    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q71, _q40 };
												    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
												      {
													{
 unsigned itype_precision = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_precision = TYPE_PRECISION (type);
 wide_int trunc_max = wi::mask (otype_precision, false, itype_precision);
 wide_int max = wi::mask (otype_precision, false, otype_precision);
 wide_int int_cst_1 = wi::to_wide (captures[1]);
 wide_int int_cst_2 = wi::to_wide (captures[2]);
													    if (wi::eq_p (trunc_max, int_cst_1) && wi::eq_p (max, int_cst_2)
)
													      {
														{
														  res_ops[0] = captures[0];
														  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 77, __FILE__, __LINE__, false);
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
									      }
									    break;
								          default:;
								          }
								        break;
								      }
								    default:;
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
					      CASE_CONVERT:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  switch (TREE_CODE (_q30))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q30))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case LE_EXPR:
							        {
								  tree _q40 = gimple_assign_rhs1 (_a4);
								  _q40 = do_valueize (valueize, _q40);
								  tree _q41 = gimple_assign_rhs2 (_a4);
								  _q41 = do_valueize (valueize, _q41);
								  switch (TREE_CODE (_q41))
								    {
								    case INTEGER_CST:
								      {
									switch (TREE_CODE (_q21))
									  {
									  case INTEGER_CST:
									    {
									      switch (TREE_CODE (_p1))
									        {
										case SSA_NAME:
										  if (gimple *_d5 = get_def (valueize, _p1))
										    {
										      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										        switch (gimple_assign_rhs_code (_a5))
											  {
											  CASE_CONVERT:
											    {
											      tree _q80 = gimple_assign_rhs1 (_a5);
											      _q80 = do_valueize (valueize, _q80);
											      if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
												{
												  {
												    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q21 };
												    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
												      {
													{
 unsigned itype_precision = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_precision = TYPE_PRECISION (type);
 wide_int trunc_max = wi::mask (otype_precision, false, itype_precision);
 wide_int max = wi::mask (otype_precision, false, otype_precision);
 wide_int int_cst_1 = wi::to_wide (captures[1]);
 wide_int int_cst_2 = wi::to_wide (captures[2]);
													    if (wi::eq_p (trunc_max, int_cst_1) && wi::eq_p (max, int_cst_2)
)
													      {
														{
														  res_ops[0] = captures[0];
														  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 77, __FILE__, __LINE__, false);
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
				    case INTEGER_CST:
				      {
					switch (TREE_CODE (_q21))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _q21))
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
								    case LE_EXPR:
								      {
									tree _q50 = gimple_assign_rhs1 (_a4);
									_q50 = do_valueize (valueize, _q50);
									tree _q51 = gimple_assign_rhs2 (_a4);
									_q51 = do_valueize (valueize, _q51);
									switch (TREE_CODE (_q51))
									  {
									  case INTEGER_CST:
									    {
									      switch (TREE_CODE (_p1))
									        {
										case SSA_NAME:
										  if (gimple *_d5 = get_def (valueize, _p1))
										    {
										      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										        switch (gimple_assign_rhs_code (_a5))
											  {
											  CASE_CONVERT:
											    {
											      tree _q80 = gimple_assign_rhs1 (_a5);
											      _q80 = do_valueize (valueize, _q80);
											      if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
												{
												  {
												    tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q20 };
												    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
												      {
													{
 unsigned itype_precision = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_precision = TYPE_PRECISION (type);
 wide_int trunc_max = wi::mask (otype_precision, false, itype_precision);
 wide_int max = wi::mask (otype_precision, false, otype_precision);
 wide_int int_cst_1 = wi::to_wide (captures[1]);
 wide_int int_cst_2 = wi::to_wide (captures[2]);
													    if (wi::eq_p (trunc_max, int_cst_1) && wi::eq_p (max, int_cst_2)
)
													      {
														{
														  res_ops[0] = captures[0];
														  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 77, __FILE__, __LINE__, false);
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
								        break;
								      }
								    default:;
								    }
							      }
							    break;
						          default:;
						          }
						        break;
						      }
						    default:;
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
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
					  if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0])) && single_use (captures[1])
)
					    {
					      {
 unsigned itype_precision = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_precision = TYPE_PRECISION (type);
 wide_int trunc_max = wi::mask (otype_precision, false, itype_precision);
 wide_int int_cst = wi::to_wide (captures[2], itype_precision);
						  if (otype_precision < itype_precision && wi::eq_p (trunc_max, int_cst)
)
						    {
						      {
							res_ops[0] = captures[0];
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 78, __FILE__, __LINE__, false);
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
gimple_simplify_22 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail600;
      {
	res_op->set_op (BIT_XOR_EXPR, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  if (type != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail600;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail600;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 120, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail600:;
    }
  return false;
}

bool
gimple_simplify_25 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (rop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail603;
      {
	res_op->set_op (rop, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  if (type != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail603;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[1];
	    if (type != TREE_TYPE (_o2[0])
	        && !useless_type_conversion_p (type, TREE_TYPE (_o2[0])))
	      {
		gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail603;
	      }
	    else
	      _r2 = _o2[0];
	    _o1[0] = _r2;
	  }
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[2];
	    if (type != TREE_TYPE (_o2[0])
	        && !useless_type_conversion_p (type, TREE_TYPE (_o2[0])))
	      {
		gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail603;
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail603;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 123, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail603:;
    }
  return false;
}

bool
gimple_simplify_28 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
1
)
 && (
 TYPE_PRECISION (TREE_TYPE (captures[1])) < TYPE_PRECISION (type)
 || (
1
 && TYPE_PRECISION (TREE_TYPE (captures[1])) == TYPE_PRECISION (type))
 || GET_MODE_CLASS (TYPE_MODE (type)) != MODE_INT
 || !type_has_mode_precision_p (type)
 || (
1
 && TREE_CODE (captures[3]) != INTEGER_CST
 && tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && single_use (captures[0])
 && single_use (captures[2])))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail606;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[3];
	    if (TREE_TYPE (_o1[0]) != TREE_TYPE (_o2[0])
	        && !useless_type_conversion_p (TREE_TYPE (_o1[0]), TREE_TYPE (_o2[0])))
	      {
		gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (_o1[0]), _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail606;
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  gimple_match_op tem_op (res_op->cond.any_else (), bitop, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail606;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 125, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail606:;
    }
  return false;
}

bool
gimple_simplify_33 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[1])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail613;
      {
	res_op->set_op (BIT_NOT_EXPR, type, 1);
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[2];
	    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o2[0]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail613;
	    _o1[0] = _r2;
	  }
	  _o1[1] = captures[3];
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail613;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 121, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail613:;
    }
  return false;
}

bool
gimple_simplify_36 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail616;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[2];
		if (t != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (t, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, t, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail616;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] =  wide_int_to_tree (t, c);
	      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, t, _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail616;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 134, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail616:;
      }
    }
  return false;
}

bool
gimple_simplify_39 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && tree_int_cst_sgn (captures[5]) > 0
 && (tree_nonzero_bits (captures[0]) & tree_nonzero_bits (captures[4])) == 0
)
    {
      {
 tree t = type;
 if (!TYPE_OVERFLOW_WRAPS (t))
 t = unsigned_type_for (t);
 wide_int wone = wi::one (TYPE_PRECISION (type));
 wide_int c = wi::add (wi::to_wide (captures[3]),
 wi::lshift (wone, wi::to_wide (captures[5])));
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[1])
	          || !single_use (captures[4])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail620;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[2];
		if (t != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (t, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, t, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail620;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] =  wide_int_to_tree (t, c);
	      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, t, _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail620;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 133, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail620:;
      }
    }
  return false;
}

bool
gimple_simplify_42 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail623;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		if (t != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (t, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, t, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail623;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] =  wide_int_to_tree (t, c);
	      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, t, _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail623;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 138, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail623:;
      }
    }
  return false;
}

bool
gimple_simplify_46 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[3]))
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[2])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail628;
      {
	res_op->set_op (GE_EXPR, type, 2);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[3];
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail628;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[1]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 143, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail628:;
    }
  return false;
}

bool
gimple_simplify_50 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail634;
	      {
		res_op->set_op (outer_op, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] = captures[3];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 149, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail634:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail635;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 150, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail635:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_54 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (type)
 && TYPE_OVERFLOW_WRAPS (type)
 && !integer_all_onesp (captures[1])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail639;
      {
	res_op->set_op (PLUS_EXPR, type, 2);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail639;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] =  build_minus_one_cst (type);
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 154, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail639:;
    }
  else
    {
      if (INTEGRAL_TYPE_P (type)
 && TREE_CODE (captures[1]) == INTEGER_CST
 && wi::to_wide (captures[1]) != wi::min_value (TYPE_PRECISION (type),
 SIGNED)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail640;
	  {
	    res_op->set_op (MINUS_EXPR, type, 2);
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[1];
	      _o1[1] =  build_minus_one_cst (type);
	      gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail640;
	      res_op->ops[0] = _r1;
	    }
	    res_op->ops[1] = captures[0];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 155, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail640:;
	}
    }
  return false;
}

bool
gimple_simplify_60 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail646;
		      {
			res_op->set_op (PLUS_EXPR, type, 2);
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[0];
			  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail646;
			  res_op->ops[0] = _r1;
			}
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[2];
			  {
			    tree _o2[1], _r2;
			    _o2[0] = captures[1];
			    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o2[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail646;
			    _o1[1] = _r2;
			  }
			  gimple_match_op tem_op (res_op->cond.any_else (), inner_op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  tem_op.resimplify (NULL, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			  if (!_r1) goto next_after_fail646;
			  res_op->ops[1] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 161, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail646:;
		    }
		  else
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail647;
		      {
			res_op->set_op (MINUS_EXPR, type, 2);
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[0];
			  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail647;
			  res_op->ops[0] = _r1;
			}
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[2];
			  {
			    tree _o2[1], _r2;
			    _o2[0] = captures[1];
			    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o2[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail647;
			    _o1[1] = _r2;
			  }
			  gimple_match_op tem_op (res_op->cond.any_else (), neg_inner_op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  tem_op.resimplify (NULL, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			  if (!_r1) goto next_after_fail647;
			  res_op->ops[1] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 162, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail647:;
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
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail648;
		      {
			res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[0];
			  {
			    tree _o2[2], _r2;
			    {
			      tree _o3[1], _r3;
			      _o3[0] = captures[2];
			      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, TREE_TYPE (_o1[0]), _o3[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r3) goto next_after_fail648;
			      _o2[0] = _r3;
			    }
			    _o2[1] = captures[1];
			    gimple_match_op tem_op (res_op->cond.any_else (), inner_op, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r2) goto next_after_fail648;
			    _o1[1] = _r2;
			  }
			  gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail648;
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 163, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail648:;
		    }
		  else
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail649;
		      {
			res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[0];
			  {
			    tree _o2[2], _r2;
			    {
			      tree _o3[1], _r3;
			      _o3[0] = captures[2];
			      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, TREE_TYPE (_o1[0]), _o3[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r3) goto next_after_fail649;
			      _o2[0] = _r3;
			    }
			    _o2[1] = captures[1];
			    gimple_match_op tem_op (res_op->cond.any_else (), neg_inner_op, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r2) goto next_after_fail649;
			    _o1[1] = _r2;
			  }
			  gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail649;
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 164, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail649:;
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
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail650;
				  {
				    res_op->set_op (inner_op, type, 2);
				    res_op->ops[0] = captures[0];
				    res_op->ops[1] =  cst;
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 165, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail650:;
				}
			      else
				{
				  if (INTEGRAL_TYPE_P (type)
 && wi::to_wide (cst) == wi::min_value (type)
)
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail651;
				      {
					res_op->set_op (neg_inner_op, type, 2);
					res_op->ops[0] = captures[0];
					res_op->ops[1] =  wide_int_to_tree (type, wi::to_wide (cst));
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 166, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail651:;
				    }
				  else
				    {
				      {
 tree utype = unsigned_type_for (type);
					  if (utype
)
					    {
					      gimple_seq *lseq = seq;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail652;
					      {
						res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
						{
						  tree _o1[2], _r1;
						  {
						    tree _o2[1], _r2;
						    _o2[0] = captures[0];
						    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o2[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r2) goto next_after_fail652;
						    _o1[0] = _r2;
						  }
						  {
						    tree _o2[1], _r2;
						    _o2[0] =  TREE_OVERFLOW (cst)
 ? drop_tree_overflow (cst) : cst;
						    gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o2[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r2) goto next_after_fail652;
						    _o1[1] = _r2;
						  }
						  gimple_match_op tem_op (res_op->cond.any_else (), inner_op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						  tem_op.resimplify (lseq, valueize);
						  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r1) goto next_after_fail652;
						  res_op->ops[0] = _r1;
						}
						res_op->resimplify (lseq, valueize);
						if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 167, __FILE__, __LINE__, true);
						return true;
					      }
next_after_fail652:;
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
gimple_simplify_78 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[2])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail673;
	  {
	    res_op->set_op (POPCOUNT, type, 1);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		if (utype != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail673;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[3];
		if (utype != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail673;
		  }
		else
		  _r2 = _o2[0];
		_o1[1] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail673;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 188, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail673:;
      }
    }
  return false;
}

bool
gimple_simplify_84 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[2])
              || !single_use (captures[3])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail679;
      {
	res_op->set_op (MINUS_EXPR, type, 2);
	res_op->ops[0] = captures[1];
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[4];
	  _o1[1] = captures[1];
	  gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail679;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 194, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail679:;
    }
  return false;
}

bool
gimple_simplify_87 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_OVERFLOW_SANITIZED (type) && !TYPE_OVERFLOW_TRAPS (type)
 && !TYPE_SATURATING (type) && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[2]))
 && !TYPE_OVERFLOW_TRAPS (TREE_TYPE (captures[2]))
 && !TYPE_SATURATING (TREE_TYPE (captures[2]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail682;
      {
	res_op->set_op (BIT_NOT_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  {
	    tree _o2[2], _r2;
	    _o2[0] = captures[0];
	    _o2[1] = captures[1];
	    gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail682;
	    _o1[0] = _r2;
	  }
	  if (type != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail682;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 197, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail682:;
    }
  return false;
}

bool
gimple_simplify_92 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail687;
	      {
		res_op->set_op (NOP_EXPR, type, 1);
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[0];
		    if (t1 != TREE_TYPE (_o2[0])
		        && !useless_type_conversion_p (t1, TREE_TYPE (_o2[0])))
		      {
			gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, t1, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail687;
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[1];
		    if (t1 != TREE_TYPE (_o2[0])
		        && !useless_type_conversion_p (t1, TREE_TYPE (_o2[0])))
		      {
			gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, t1, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail687;
		      }
		    else
		      _r2 = _o2[0];
		    _o1[1] = _r2;
		  }
		  gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail687;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 201, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail687:;
	  }
	}
    }
  return false;
}

bool
gimple_simplify_98 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail695;
		  {
		    res_op->set_op (PLUS_EXPR, type, 2);
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[2];
		      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail695;
		      res_op->ops[0] = _r1;
		    }
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[0];
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[1];
			gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail695;
			_o1[1] = _r2;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      tem_op.resimplify (NULL, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, NULL);
		      if (!_r1) goto next_after_fail695;
		      res_op->ops[1] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 209, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail695:;
		}
	    }
	  else
	    {
	      if (!ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[2]))
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail696;
		  {
		    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[2];
		      {
			tree _o2[2], _r2;
			{
			  tree _o3[1], _r3;
			  _o3[0] = captures[0];
			  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, TREE_TYPE (_o1[0]), _o3[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r3) goto next_after_fail696;
			  _o2[0] = _r3;
			}
			_o2[1] = captures[1];
			gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			tem_op.resimplify (NULL, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, NULL);
			if (!_r2) goto next_after_fail696;
			_o1[1] = _r2;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail696;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 210, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail696:;
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
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail697;
			      {
				res_op->set_op (PLUS_EXPR, type, 2);
				res_op->ops[0] =  cst;
				res_op->ops[1] = captures[2];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 211, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail697:;
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
gimple_simplify_106 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (log1),
 const enum tree_code ARG_UNUSED (log2),
 const combined_fn ARG_UNUSED (popcount))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])
          || !single_use (captures[1])
          || !single_use (captures[3])
          || !single_use (captures[5])
          || !single_use (captures[6])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail707;
  {
    res_op->set_op (popcount, type, 1);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[2];
      _o1[1] = captures[4];
      gimple_match_op tem_op (res_op->cond.any_else (), log2, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail707;
      res_op->ops[0] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 221, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail707:;
  return false;
}

bool
gimple_simplify_109 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == TYPE_PRECISION (TREE_TYPE (captures[2]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail710;
      {
	res_op->set_op (cmp, type, 2);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[2];
	    if (TREE_TYPE (_o1[0]) != TREE_TYPE (_o2[0])
	        && !useless_type_conversion_p (TREE_TYPE (_o1[0]), TREE_TYPE (_o2[0])))
	      {
		gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (_o1[0]), _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail710;
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail710;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 224, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail710:;
    }
  return false;
}

bool
gimple_simplify_117 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail718;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 231, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail718:;
  return false;
}

bool
gimple_simplify_121 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail722;
  {
    res_op->set_op (BIT_IOR_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[3];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 234, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail722:;
  return false;
}

bool
gimple_simplify_126 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail727;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 236, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail727:;
  return false;
}

bool
gimple_simplify_130 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])
          || !single_use (captures[3])
          || !single_use (captures[4])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail731;
  {
    res_op->set_op (BIT_NOT_EXPR, type, 1);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail731;
      res_op->ops[0] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 239, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail731:;
  return false;
}

bool
gimple_simplify_136 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail739;
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
		  if (!_r1) goto next_after_fail739;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 242, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail739:;
	}
      else
	{
	  if (single_use (captures[0]) && single_use (captures[1])
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail740;
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
		      if (!_r1) goto next_after_fail740;
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
next_after_fail740:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_142 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[4], captures[7])
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[1])
              || !single_use (captures[2])
              || !single_use (captures[3])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail747;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[2], _r2;
	    _o2[0] = captures[5];
	    {
	      tree _o3[1], _r3;
	      _o3[0] = captures[6];
	      if (TREE_TYPE (_o2[0]) != TREE_TYPE (_o3[0])
	          && !useless_type_conversion_p (TREE_TYPE (_o2[0]), TREE_TYPE (_o3[0])))
		{
		  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (_o2[0]), _o3[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r3) goto next_after_fail747;
	        }
	      else
	        _r3 = _o3[0];
	      _o2[1] = _r3;
	    }
	    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail747;
	    _o1[0] = _r2;
	  }
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[4];
	    if (TREE_TYPE (_o1[0]) != TREE_TYPE (_o2[0])
	        && !useless_type_conversion_p (TREE_TYPE (_o1[0]), TREE_TYPE (_o2[0])))
	      {
		gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (_o1[0]), _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail747;
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail747;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 248, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail747:;
    }
  return false;
}

bool
gimple_simplify_150 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (captures[5])
 && invert_tree_comparison (cmp, HONOR_NANS (captures[2])) == icmp
)
    {
      if (integer_onep (captures[4])
)
	{
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[7])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail756;
	  {
	    res_op->set_op (BIT_AND_EXPR, type, 2);
	    {
	      tree _o1[3], _r1;
	      _o1[0] = captures[1];
	      _o1[1] = captures[6];
	      _o1[2] = captures[9];
	      gimple_match_op tem_op (res_op->cond.any_else (), VEC_COND_EXPR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail756;
	      res_op->ops[0] = _r1;
	    }
	    res_op->ops[1] = captures[4];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 252, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail756:;
	}
      else
	{
	  if (integer_minus_onep (captures[4])
)
	    {
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])
	              || !single_use (captures[7])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail757;
	      {
		res_op->set_op (VEC_COND_EXPR, type, 3);
		res_op->ops[0] = captures[1];
		res_op->ops[1] = captures[6];
		res_op->ops[2] = captures[9];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 253, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail757:;
	    }
	}
    }
  else
    {
      if (integer_zerop (captures[4])
 && invert_tree_comparison (cmp, HONOR_NANS (captures[2])) == icmp
)
	{
	  if (integer_onep (captures[5])
)
	    {
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])
	              || !single_use (captures[7])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail758;
	      {
		res_op->set_op (BIT_AND_EXPR, type, 2);
		{
		  tree _o1[3], _r1;
		  _o1[0] = captures[1];
		  _o1[1] = captures[9];
		  _o1[2] = captures[6];
		  gimple_match_op tem_op (res_op->cond.any_else (), VEC_COND_EXPR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail758;
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] = captures[5];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 254, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail758:;
	    }
	  else
	    {
	      if (integer_minus_onep (captures[5])
)
		{
		  gimple_seq *lseq = seq;
		  if (lseq
		      && (!single_use (captures[0])
		          || !single_use (captures[7])))
		    lseq = NULL;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail759;
		  {
		    res_op->set_op (VEC_COND_EXPR, type, 3);
		    res_op->ops[0] = captures[1];
		    res_op->ops[1] = captures[9];
		    res_op->ops[2] = captures[6];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 255, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail759:;
		}
	    }
	}
    }
  return false;
}

bool
gimple_simplify_160 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail787;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 257, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail787:;
  return false;
}

bool
gimple_simplify_161 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail788;
  {
    tree tem;
    tem =  constant_boolean_node (true, type);
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 276, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail788:;
  return false;
}

bool
gimple_simplify_164 (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail792;
	      {
		tree tem;
		tem = captures[3];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 260, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail792:;
	    }
	  else
	    {
	      if (code1 == NE_EXPR && val && allbits
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail793;
		  {
		    tree tem;
		    tem =  constant_boolean_node (true, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 261, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail793:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail794;
		      {
			tree tem;
			tem = captures[0];
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 262, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail794:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail795;
			  {
			    res_op->set_op (GE_EXPR, type, 2);
			    res_op->ops[0] = captures[4];
			    res_op->ops[1] = captures[5];
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 263, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail795:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail796;
			      {
				res_op->set_op (LE_EXPR, type, 2);
				res_op->ops[0] = captures[4];
				res_op->ops[1] = captures[5];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 264, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail796:;
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail797;
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
					  if (!_r1) goto next_after_fail797;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 265, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail797:;
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
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail798;
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
					      if (!_r1) goto next_after_fail798;
					    }
					  else
					    _r1 = _o1[0];
					  res_op->ops[1] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 266, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail798:;
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
gimple_simplify_181 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail822;
      {
	res_op->set_op (NEGATE_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail822;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 282, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail822:;
    }
  return false;
}

bool
gimple_simplify_186 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail827;
      {
	res_op->set_op (NEGATE_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail827;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 283, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail827:;
    }
  return false;
}

bool
gimple_simplify_191 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail832;
      {
	res_op->set_op (NEGATE_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail832;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 285, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail832:;
    }
  return false;
}

bool
gimple_simplify_196 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail837;
  {
    tree tem;
    tem =  build_zero_cst (type);
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 289, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail837:;
  return false;
}

bool
gimple_simplify_201 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail844;
	  {
	    res_op->set_op (COND_EXPR, type, 3);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] = captures[3];
	    res_op->ops[2] =  build_zero_cst (type);
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 293, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail844:;
	}
    }
  return false;
}

bool
gimple_simplify_204 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
 && tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[2]))
)
    {
      {
 wi::overflow_type overflow;
 wide_int mul = wi::mul (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (type), &overflow);
	  if (types_match (type, TREE_TYPE (captures[2]))
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[2])) && !overflow
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail847;
	      {
		res_op->set_op (op, type, 2);
		res_op->ops[0] = captures[0];
		res_op->ops[1] =  wide_int_to_tree (type, mul);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 295, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail847:;
	    }
	  else
	    {
	      {
 tree utype = unsigned_type_for (type);
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail848;
		  {
		    res_op->set_op (NOP_EXPR, type, 1);
		    {
		      tree _o1[2], _r1;
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[0];
			if (utype != TREE_TYPE (_o2[0])
			    && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
			  {
			    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail848;
			  }
			else
			  _r2 = _o2[0];
			_o1[0] = _r2;
		      }
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
			      if (!_r3) goto next_after_fail848;
			    }
			  else
			    _r3 = _o3[0];
			  _o2[0] = _r3;
			}
			{
			  tree _o3[1], _r3;
			  _o3[0] = captures[2];
			  if (utype != TREE_TYPE (_o3[0])
			      && !useless_type_conversion_p (utype, TREE_TYPE (_o3[0])))
			    {
			      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o3[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r3) goto next_after_fail848;
			    }
			  else
			    _r3 = _o3[0];
			  _o2[1] = _r3;
			}
			gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail848;
			_o1[1] = _r2;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail848;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 296, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail848:;
	      }
	    }
      }
    }
  return false;
}

bool
gimple_simplify_219 (gimple_match_op *res_op, gimple_seq *seq,
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail866;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[2];
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[2]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 313, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail866:;
    }
  return false;
}

bool
gimple_simplify_223 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail883;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[2];
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[2]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 330, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail883:;
    }
  return false;
}

bool
gimple_simplify_226 (gimple_match_op *res_op, gimple_seq *seq,
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail886;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[3];
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[3]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 329, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail886:;
    }
  return false;
}

bool
gimple_simplify_230 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_INT_CST_LOW (captures[1]) & 1
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail890;
      {
	tree tem;
	tem =  constant_boolean_node (cmp == NE_EXPR, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 336, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail890:;
    }
  return false;
}

bool
gimple_simplify_231 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::bit_and_not (wi::to_wide (captures[1]), get_nonzero_bits (captures[0])) != 0
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail891;
      {
	tree tem;
	tem =  constant_boolean_node (cmp == NE_EXPR, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 337, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail891:;
    }
  return false;
}

bool
gimple_simplify_235 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::gt_p (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (TREE_TYPE (captures[0])))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail896;
      {
	tree tem;
	tem =  constant_boolean_node (cmp == NE_EXPR, type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 342, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail896:;
    }
  else
    {
      if (wi::lt_p (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (TREE_TYPE (captures[0])))
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail897;
	  {
	    res_op->set_op (cmp, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] = captures[2];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 343, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail897:;
	}
    }
  return false;
}

bool
gimple_simplify_239 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail903;
	      {
		tree tem;
		tem =  constant_boolean_node (cmp == NE_EXPR ? false : true, type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 349, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail903:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail904;
	      {
		res_op->set_op (icmp, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] =  build_int_cst (TREE_TYPE (captures[1]), c2 - c1);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 350, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail904:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_243 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rotate),
 const enum tree_code ARG_UNUSED (invrot),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail915;
  {
    res_op->set_op (cmp, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[2];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 361, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail915:;
  return false;
}

bool
gimple_simplify_245 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rotate),
 const enum tree_code ARG_UNUSED (invrot),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (captures[2]) || integer_all_onesp (captures[2])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail917;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 363, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail917:;
    }
  return false;
}

bool
gimple_simplify_248 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree ctype = TREE_TYPE (captures[1]);
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail920;
      {
	res_op->set_op (cmp, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  if (ctype != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (ctype, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, ctype, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail920;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  gimple_match_op tem_op (res_op->cond.any_else (), bswap, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (NULL, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, NULL);
	  if (!_r1) goto next_after_fail920;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 366, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail920:;
  }
  return false;
}

bool
gimple_simplify_254 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail929;
	      {
		res_op->set_op (cmp, type, 2);
		res_op->ops[0] = captures[0];
		res_op->ops[1] =  wide_int_to_tree (TREE_TYPE (captures[0]), quot);
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 374, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail929:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail930;
	      {
		tree tem;
		tem =  constant_boolean_node (cmp == NE_EXPR, type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 375, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail930:;
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail931;
	  {
	    res_op->set_op (cmp, type, 2);
	    res_op->ops[0] = captures[0];
 tree itype = TREE_TYPE (captures[0]);
 int p = TYPE_PRECISION (itype);
 wide_int m = wi::one (p + 1) << p;
 wide_int a = wide_int::from (wi::to_wide (captures[1]), p + 1, UNSIGNED);
 wide_int i = wide_int::from (wi::mod_inv (a, m),
 p, TYPE_SIGN (itype));	    res_op->ops[1] = 
 wide_int_to_tree (itype, wi::mul (i, wi::to_wide (captures[2])));
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 376, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail931:;
	}
    }
  return false;
}

bool
gimple_simplify_263 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail955;
	      {
		tree tem;
		tem =  constant_boolean_node (false, type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 399, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail955:;
	    }
	  else
	    {
	      if (cmp == NE_EXPR || !HONOR_NANS (captures[0])
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail956;
		  {
		    tree tem;
		    tem =  constant_boolean_node (true, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 400, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail956:;
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail957;
		  {
		    res_op->set_op (GE_EXPR, type, 2);
		    res_op->ops[0] = captures[0];
		    res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 401, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail957:;
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail958;
		  {
		    tree tem;
		    tem =  constant_boolean_node (false, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 402, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail958:;
		}
	      else
		{
		  if (cmp == GE_EXPR && !HONOR_NANS (captures[0])
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail959;
		      {
			tree tem;
			tem =  constant_boolean_node (true, type);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 403, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail959:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail960;
			  {
			    res_op->set_op (EQ_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] = captures[1];
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 404, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail960:;
			}
		      else
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail961;
			  {
			    res_op->set_op (cmp, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] = captures[1];
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 405, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail961:;
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
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail962;
				  {
				    res_op->set_op (EQ_EXPR, type, 2);
				    res_op->ops[0] = captures[0];
				    res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), c2);
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 406, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail962:;
				}
			      else
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail963;
				  {
				    tree tem;
				    tem =  constant_boolean_node (false, type);
				    res_op->set_value (tem);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 407, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail963:;
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
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail964;
				      {
					res_op->set_op (GE_EXPR, type, 2);
					res_op->ops[0] = captures[0];
					res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), c2);
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 408, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail964:;
				    }
				  else
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail965;
				      {
					res_op->set_op (GT_EXPR, type, 2);
					res_op->ops[0] = captures[0];
					res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), c2);
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 409, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail965:;
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
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail966;
				  {
				    tree tem;
				    tem =  constant_boolean_node (true, type);
				    res_op->set_value (tem);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 410, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail966:;
				}
			      else
				{
				  if (! HONOR_NANS (captures[0])
)
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail967;
				      {
					res_op->set_op (NE_EXPR, type, 2);
					res_op->ops[0] = captures[0];
					res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), c2);
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 411, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail967:;
				    }
				  else
				    {
				      if (! HONOR_INFINITIES (captures[0])
)
					{
					  gimple_seq *lseq = seq;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail968;
					  {
					    res_op->set_op (GE_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 412, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail968:;
					}
				      else
					{
					  if (0
)
					    {
					      gimple_seq *lseq = seq;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail969;
					      {
						res_op->set_op (TRUTH_ANDIF_EXPR, type, 2);
						{
						  tree _o1[2], _r1;
						  _o1[0] = captures[0];
						  _o1[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
						  gimple_match_op tem_op (res_op->cond.any_else (), GE_EXPR, type, _o1[0], _o1[1]);
						  tem_op.resimplify (lseq, valueize);
						  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r1) goto next_after_fail969;
						  res_op->ops[0] = _r1;
						}
						{
						  tree _o1[2], _r1;
						  _o1[0] = captures[0];
						  _o1[1] =  build_real (TREE_TYPE (captures[0]), c2);
						  gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o1[0], _o1[1]);
						  tem_op.resimplify (lseq, valueize);
						  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r1) goto next_after_fail969;
						  res_op->ops[1] = _r1;
						}
						res_op->resimplify (lseq, valueize);
						if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 413, __FILE__, __LINE__, true);
						return true;
					      }
next_after_fail969:;
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
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail970;
				      {
					res_op->set_op (LT_EXPR, type, 2);
					res_op->ops[0] = captures[0];
					res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), c2);
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 414, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail970:;
				    }
				  else
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail971;
				      {
					res_op->set_op (LE_EXPR, type, 2);
					res_op->ops[0] = captures[0];
					res_op->ops[1] =  build_real (TREE_TYPE (captures[0]), c2);
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 415, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail971:;
				    }
				}
			      else
				{
				  if (ncmp != ERROR_MARK &&
0
)
				    {
				      if (ncmp == LT_EXPR
)
					{
					  gimple_seq *lseq = seq;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail972;
					  {
					    res_op->set_op (TRUTH_ANDIF_EXPR, type, 2);
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[0];
					      _o1[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
					      gimple_match_op tem_op (res_op->cond.any_else (), GE_EXPR, type, _o1[0], _o1[1]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail972;
					      res_op->ops[0] = _r1;
					    }
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[0];
					      _o1[1] =  build_real (TREE_TYPE (captures[0]), c2);
					      gimple_match_op tem_op (res_op->cond.any_else (), LT_EXPR, type, _o1[0], _o1[1]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail972;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 416, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail972:;
					}
				      else
					{
					  gimple_seq *lseq = seq;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail973;
					  {
					    res_op->set_op (TRUTH_ANDIF_EXPR, type, 2);
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[0];
					      _o1[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
					      gimple_match_op tem_op (res_op->cond.any_else (), GE_EXPR, type, _o1[0], _o1[1]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail973;
					      res_op->ops[0] = _r1;
					    }
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[0];
					      _o1[1] =  build_real (TREE_TYPE (captures[0]), c2);
					      gimple_match_op tem_op (res_op->cond.any_else (), LE_EXPR, type, _o1[0], _o1[1]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail973;
					      res_op->ops[1] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 417, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail973:;
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
gimple_simplify_297 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp),
 const combined_fn ARG_UNUSED (clz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (captures[2]) && single_use (captures[0])
)
    {
      {
 tree stype = signed_type_for (TREE_TYPE (captures[1]));
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1058;
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
		  if (!_r1) goto next_after_fail1058;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[0] = _r1;
	    }
	    res_op->ops[1] =  build_zero_cst (stype);
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 485, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1058:;
      }
    }
  else
    {
      if (wi::to_wide (captures[2]) == TYPE_PRECISION (TREE_TYPE (captures[1])) - 1
)
	{
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1059;
	  {
	    res_op->set_op (op, type, 2);
	    res_op->ops[0] = captures[1];
	    res_op->ops[1] =  build_one_cst (TREE_TYPE (captures[1]));
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 486, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1059:;
	}
    }
  return false;
}

bool
gimple_simplify_303 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (captures[0])))
 && can_compare_p (cmp == EQ_EXPR ? EQ : NE, TYPE_MODE (TREE_TYPE (captures[0])),
 ccp_jump)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1070;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[0]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 497, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1070:;
    }
  return false;
}

bool
gimple_simplify_307 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (REAL_VALUE_NEGATIVE (TREE_REAL_CST (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1075;
      {
	res_op->set_op (COPYSIGN_ALL, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1075;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 502, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1075:;
    }
  return false;
}

bool
gimple_simplify_311 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1080;
  {
    tree tem;
    tem = captures[1];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 507, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1080:;
  return false;
}

bool
gimple_simplify_316 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_UNSIGNED (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1087;
      {
	res_op->set_op (NEGATE_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 512, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1087:;
    }
  return false;
}

bool
gimple_simplify_320 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((INTEGRAL_TYPE_P (type) || VECTOR_INTEGER_TYPE_P (type))
 && TYPE_OVERFLOW_UNDEFINED (type)
 && !integer_zerop (captures[0])
 && (!flag_non_call_exceptions || tree_expr_nonzero_p (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1091;
      {
	tree tem;
	tem =  build_minus_one_cst (type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 516, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1091:;
    }
  return false;
}

bool
gimple_simplify_323 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (type)
)
    {
      if (TYPE_OVERFLOW_UNDEFINED (type) && !TYPE_OVERFLOW_SANITIZED (type)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1095;
	  {
	    tree tem;
	    tem = captures[0];
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 520, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1095:;
	}
      else
	{
	  {
int_range_max vr0, vr1;
	      if (INTEGRAL_TYPE_P (type)
 && get_range_query (cfun)->range_of_expr (vr0, captures[0])
 && get_range_query (cfun)->range_of_expr (vr1, captures[1])
 && range_op_handler (MULT_EXPR).overflow_free_p (vr0, vr1)
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1096;
		  {
		    tree tem;
		    tem = captures[0];
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 521, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1096:;
		}
	  }
	}
    }
  return false;
}

bool
gimple_simplify_329 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!integer_zerop (captures[1])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1104;
      {
	tree tem;
	tem = captures[0];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 529, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1104:;
    }
  return false;
}

bool
gimple_simplify_334 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (type)
 && TYPE_OVERFLOW_UNDEFINED (type)
 && wi::multiple_of_p (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (type))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1109;
      {
	tree tem;
	tem =  build_zero_cst (type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 534, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1109:;
    }
  return false;
}

bool
gimple_simplify_341 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SINH),
 const combined_fn ARG_UNUSED (COSH),
 const combined_fn ARG_UNUSED (TANH))
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1118;
      {
	res_op->set_op (TANH, type, 1);
	res_op->ops[0] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 543, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1118:;
    }
  return false;
}

bool
gimple_simplify_346 (gimple_match_op *res_op, gimple_seq *seq,
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
          && (!single_use (captures[1])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1123;
      {
	res_op->set_op (MULT_EXPR, type, 2);
	res_op->ops[0] = captures[0];
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[2];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[3];
	    gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail1123;
	    _o1[1] = _r2;
	  }
	  gimple_match_op tem_op (res_op->cond.any_else (), POW, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1123;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 548, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1123:;
    }
  return false;
}

bool
gimple_simplify_353 (gimple_match_op *res_op, gimple_seq *seq,
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1132;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 557, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1132:;
    }
  return false;
}

bool
gimple_simplify_358 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (acmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree cst = uniform_integer_cst_p (captures[1]);
      if (tree_int_cst_sgn (cst) == 1
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1138;
	  {
	    res_op->set_op (acmp, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] =  build_uniform_cst (TREE_TYPE (captures[1]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst) - 1));
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 563, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1138:;
	}
  }
  return false;
}

bool
gimple_simplify_363 (gimple_match_op *res_op, gimple_seq *seq,
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1160;
		  {
		    tree tem;
		    tem =  constant_boolean_node (false, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 569, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1160:;
		}
	      else
		{
		  if (cmp == GE_EXPR
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1161;
		      {
			res_op->set_op (EQ_EXPR, type, 2);
			res_op->ops[0] = captures[0];
			res_op->ops[1] = captures[2];
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 570, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1161:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1162;
			  {
			    tree tem;
			    tem =  constant_boolean_node (true, type);
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 571, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1162:;
			}
		      else
			{
			  if (cmp == LT_EXPR
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1163;
			      {
				res_op->set_op (NE_EXPR, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] = captures[2];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 572, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1163:;
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
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1164;
		      {
			tree tem;
			tem =  constant_boolean_node (false, type);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 573, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1164:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1165;
			  {
			    res_op->set_op (EQ_EXPR, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] = captures[2];
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 574, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1165:;
			}
		      else
			{
			  if (cmp == GE_EXPR
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1166;
			      {
				tree tem;
				tem =  constant_boolean_node (true, type);
				res_op->set_value (tem);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 575, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1166:;
			    }
			  else
			    {
			      if (cmp == GT_EXPR
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1167;
				  {
				    res_op->set_op (NE_EXPR, type, 2);
				    res_op->ops[0] = captures[0];
				    res_op->ops[1] = captures[2];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 576, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1167:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1168;
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
next_after_fail1168:;
			}
		      else
			{
			  if (cmp == LE_EXPR
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1169;
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
next_after_fail1169:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1170;
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
next_after_fail1170:;
			    }
			  else
			    {
			      if (cmp == LT_EXPR
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1171;
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
next_after_fail1171:;
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
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1172;
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
						  if (!_r1) goto next_after_fail1172;
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
next_after_fail1172:;
					}
				      else
					{
					  if (cst == captures[2] && cmp == GT_EXPR
)
					    {
					      gimple_seq *lseq = seq;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1173;
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
						      if (!_r1) goto next_after_fail1173;
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
next_after_fail1173:;
					    }
					  else
					    {
					      if (cmp == LE_EXPR
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1174;
						  {
						    res_op->set_op (GE_EXPR, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, st, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail1174;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  build_zero_cst (st);
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 583, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail1174:;
						}
					      else
						{
						  if (cmp == GT_EXPR
)
						    {
						      gimple_seq *lseq = seq;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1175;
						      {
							res_op->set_op (LT_EXPR, type, 2);
							{
							  tree _o1[1], _r1;
							  _o1[0] = captures[1];
							  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, st, _o1[0]);
							  tem_op.resimplify (lseq, valueize);
							  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
							  if (!_r1) goto next_after_fail1175;
							  res_op->ops[0] = _r1;
							}
							res_op->ops[1] =  build_zero_cst (st);
							res_op->resimplify (lseq, valueize);
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 584, __FILE__, __LINE__, true);
							return true;
						      }
next_after_fail1175:;
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
gimple_simplify_400 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (froms),
 const combined_fn ARG_UNUSED (tos))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (optimize
 && types_match (type, TREE_TYPE (captures[0]))
 && direct_internal_fn_supported_p (as_internal_fn (tos),
 type, OPTIMIZE_FOR_BOTH)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1232;
      {
	res_op->set_op (tos, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 634, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1232:;
    }
  return false;
}

bool
gimple_simplify_403 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (maxmin))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (optimize
 && FLOAT_TYPE_P (type)
 && FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 && types_match (type, TREE_TYPE (captures[1]))
 && types_match (type, TREE_TYPE (captures[2]))
 && element_precision (type) < element_precision (TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1235;
      {
	res_op->set_op (maxmin, type, 2);
	res_op->ops[0] = captures[1];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 637, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1235:;
    }
  return false;
}

bool
gimple_simplify_407 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1248;
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
		  if (!_r1) goto next_after_fail1248;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 638, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1248:;
	}
      else
	{
	  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1249;
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
			if (!_r2) goto next_after_fail1249;
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
			if (!_r2) goto next_after_fail1249;
		      }
		    else
		      _r2 = _o2[0];
		    _o1[1] = _r2;
		  }
		  gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1249;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 639, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1249:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1250;
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
				  if (!_r1) goto next_after_fail1250;
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
				  if (!_r1) goto next_after_fail1250;
			        }
			      else
			        _r1 = _o1[0];
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 640, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1250:;
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1251;
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
					    if (!_r2) goto next_after_fail1251;
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
					    if (!_r2) goto next_after_fail1251;
					  }
					else
					  _r2 = _o2[0];
					_o1[1] = _r2;
				      }
				      gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1251;
				      res_op->ops[0] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 641, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1251:;
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
gimple_simplify_430 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1276;
  {
    res_op->set_op (BIT_AND_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 666, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1276:;
  return false;
}

bool
gimple_simplify_437 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1283;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		{
		  tree _o3[1], _r3;
		  _o3[0] =  pmop[0];
		  if (utype != TREE_TYPE (_o3[0])
		      && !useless_type_conversion_p (utype, TREE_TYPE (_o3[0])))
		    {
		      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o3[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r3) goto next_after_fail1283;
		    }
		  else
		    _r3 = _o3[0];
		  _o2[0] = _r3;
		}
		gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1283;
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
		    if (!_r2) goto next_after_fail1283;
		  }
		else
		  _r2 = _o2[0];
		_o1[1] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1283;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 672, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1283:;
	}
  }
  return false;
}

bool
gimple_simplify_449 (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1297;
	      {
		tree tem;
		tem =  build_int_cst (type, 0);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 682, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1297:;
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
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1298;
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
						      if (!_r3) goto next_after_fail1298;
						    }
						  else
						    _r3 = _o3[0];
						  _o2[0] = _r3;
						}
						_o2[1] = captures[4];
						gimple_match_op tem_op (res_op->cond.any_else (), shift, shift_type, _o2[0], _o2[1]);
						tem_op.resimplify (lseq, valueize);
						_r2 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r2) goto next_after_fail1298;
						_o1[0] = _r2;
					      }
					      if (type != TREE_TYPE (_o1[0])
					          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
						{
						  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
						  tem_op.resimplify (lseq, valueize);
						  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						  if (!_r1) goto next_after_fail1298;
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
next_after_fail1298:;
					}
				      else
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[0])
					          || !single_use (captures[1])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1299;
					  {
					    res_op->set_op (BIT_AND_EXPR, type, 2);
					    res_op->ops[0] = captures[0];
					    res_op->ops[1] =  newmaskt;
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 684, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail1299:;
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
gimple_simplify_466 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1]))
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[2])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1351;
      {
	res_op->set_op (GT_EXPR, type, 2);
	res_op->ops[0] = captures[3];
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] =  build_int_cst (TREE_TYPE (captures[1]), 1);
	  gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1351;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 708, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1351:;
    }
  return false;
}

bool
gimple_simplify_471 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((wi::to_wide (captures[2]) & 1) != 0
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1361;
      {
	tree tem;
	tem = captures[0];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 710, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1361:;
    }
  else
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1362;
      {
	tree tem;
	tem =  build_zero_cst (type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 711, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1362:;
    }
  return false;
}

bool
gimple_simplify_474 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1365;
  {
    tree tem;
    tem = captures[1];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 713, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1365:;
  return false;
}

bool
gimple_simplify_478 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1370;
  {
    res_op->set_op (BIT_AND_EXPR, type, 2);
    res_op->ops[0] = captures[1];
    {
      tree _o1[1], _r1;
      _o1[0] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1370;
      res_op->ops[1] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 718, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1370:;
  return false;
}

bool
gimple_simplify_481 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (element_precision (type) <= element_precision (TREE_TYPE (captures[0]))
 || !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1373;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1373;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 721, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1373:;
    }
  return false;
}

bool
gimple_simplify_484 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1376;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[1];
	    gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail1376;
	    _o1[1] = _r2;
	  }
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1376;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 724, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1376:;
    }
  return false;
}

bool
gimple_simplify_492 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1385;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 731, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1385:;
  return false;
}

bool
gimple_simplify_495 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_CODE (type) != COMPLEX_TYPE
 && (! ANY_INTEGRAL_TYPE_P (type)
 || TYPE_OVERFLOW_UNDEFINED (type))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1388;
      {
	res_op->set_op (NEGATE_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1388;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 733, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1388:;
    }
  return false;
}

bool
gimple_simplify_503 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_CODE (type) != COMPLEX_TYPE
 && (! ANY_INTEGRAL_TYPE_P (type)
 || TYPE_OVERFLOW_UNDEFINED (type))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1408;
      {
	res_op->set_op (ABS_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 741, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1408:;
    }
  return false;
}

bool
gimple_simplify_507 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (FMAX_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_finite_math_only
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1412;
      {
	res_op->set_op (MAX_EXPR, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 745, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1412:;
    }
  return false;
}

bool
gimple_simplify_512 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1417;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 750, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1417:;
  return false;
}

bool
gimple_simplify_519 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1424;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[0];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[2];
		if (TREE_TYPE (_o1[0]) != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (TREE_TYPE (_o1[0]), TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (_o1[0]), _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail1424;
		  }
		else
		  _r2 = _o2[0];
		_o1[1] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1424;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 757, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1424:;
	}
      else
	{
	  if (code == MIN_EXPR
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1425;
	      {
		res_op->set_op (NOP_EXPR, type, 1);
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[2];
		    if (TREE_TYPE (_o1[0]) != TREE_TYPE (_o2[0])
		        && !useless_type_conversion_p (TREE_TYPE (_o1[0]), TREE_TYPE (_o2[0])))
		      {
			gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (_o1[0]), _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1425;
		      }
		    else
		      _r2 = _o2[0];
		    _o1[1] = _r2;
		  }
		  gimple_match_op tem_op (res_op->cond.any_else (), MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1425;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 758, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1425:;
	    }
	  else
	    {
	      if (code == EQ_EXPR
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1426;
		  {
		    res_op->set_op (NOP_EXPR, type, 1);
		    {
		      tree _o1[3], _r1;
		      {
			tree _o2[2], _r2;
			_o2[0] = captures[0];
			{
			  tree _o3[1], _r3;
			  _o3[0] = captures[1];
			  if (TREE_TYPE (_o2[0]) != TREE_TYPE (_o3[0])
			      && !useless_type_conversion_p (TREE_TYPE (_o2[0]), TREE_TYPE (_o3[0])))
			    {
			      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, TREE_TYPE (_o2[0]), _o3[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r3) goto next_after_fail1426;
			    }
			  else
			    _r3 = _o3[0];
			  _o2[1] = _r3;
			}
			gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, boolean_type_node, _o2[0], _o2[1]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1426;
			_o1[0] = _r2;
		      }
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[1];
			if (from_type != TREE_TYPE (_o2[0])
			    && !useless_type_conversion_p (from_type, TREE_TYPE (_o2[0])))
			  {
			    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, from_type, _o2[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail1426;
			  }
			else
			  _r2 = _o2[0];
			_o1[1] = _r2;
		      }
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[2];
			if (from_type != TREE_TYPE (_o2[0])
			    && !useless_type_conversion_p (from_type, TREE_TYPE (_o2[0])))
			  {
			    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, from_type, _o2[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail1426;
			  }
			else
			  _r2 = _o2[0];
			_o1[2] = _r2;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), COND_EXPR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1426;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 759, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1426:;
		}
	    }
	}
  }
  return false;
}

bool
gimple_simplify_537 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::to_wide (uniform_integer_cst_p (captures[3])) == 2
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1492;
      {
	tree tem;
	tem = captures[2];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 784, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1492:;
    }
  return false;
}

bool
gimple_simplify_540 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::eq_p (wi::bit_not (wi::to_wide (captures[1])), wi::to_wide (captures[3]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1495;
      {
	tree tem;
	tem = captures[2];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 786, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1495:;
    }
  return false;
}

bool
gimple_simplify_544 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1501;
      {
	tree tem;
	tem = captures[2];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 790, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1501:;
    }
  return false;
}

bool
gimple_simplify_547 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[2])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1504;
      {
	tree tem;
	tem = captures[1];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 793, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1504:;
    }
  return false;
}

bool
gimple_simplify_554 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[1])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1511;
      {
	tree tem;
	tem = captures[1];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 797, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1511:;
    }
  return false;
}

bool
gimple_simplify_562 (gimple_match_op *res_op, gimple_seq *seq,
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
      if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
	{
	  {
 tree stype = signed_type_for (TREE_TYPE (captures[2]));
	      if (types_match (captures[0], stype)
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1522;
		  {
		    res_op->set_op (ABSU_EXPR, type, 1);
		    res_op->ops[0] = captures[0];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 802, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1522:;
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1523;
		  {
		    res_op->set_op (ABSU_EXPR, type, 1);
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[2];
		      if (stype != TREE_TYPE (_o1[0])
		          && !useless_type_conversion_p (stype, TREE_TYPE (_o1[0])))
			{
			  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, stype, _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail1523;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 803, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1523:;
		}
	  }
	}
      else
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1524;
	  {
	    res_op->set_op (ABS_EXPR, type, 1);
	    res_op->ops[0] = captures[2];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 804, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1524:;
	}
    }
  return false;
}

bool
gimple_simplify_571 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1543;
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
		    if (!_r2) goto next_after_fail1543;
		    _o1[0] = _r2;
		  }
		  gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1543;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 813, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1543:;
	  }
	}
      else
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1544;
	  {
	    res_op->set_op (NEGATE_EXPR, type, 1);
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[0];
	      gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1544;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 814, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1544:;
	}
    }
  return false;
}

bool
gimple_simplify_577 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (func))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1557;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 819, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1557:;
  return false;
}

bool
gimple_simplify_580 (gimple_match_op *res_op, gimple_seq *seq,
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
 ifn = IFN_CLZ;
 }
 }
 }
 else if (direct_internal_fn_supported_p (IFN_CLZ, TREE_TYPE (captures[2]),
 OPTIMIZE_FOR_BOTH)
 && CLZ_DEFINED_VALUE_AT_ZERO
 (SCALAR_INT_TYPE_MODE (TREE_TYPE (captures[2])), val) == 2)
 ifn = IFN_CLZ;
      if (ifn == IFN_CLZ && wi::to_widest (captures[3]) == val
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1560;
	  {
	    res_op->set_op (CFN_CLZ, type, 2);
	    res_op->ops[0] = captures[2];
	    res_op->ops[1] = captures[3];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 821, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1560:;
	}
  }
  return false;
}

bool
gimple_simplify_585 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1565;
	  {
	    res_op->set_op (CFN_CTZ, type, 2);
	    res_op->ops[0] = captures[2];
	    res_op->ops[1] = captures[3];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 823, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1565:;
	}
  }
  return false;
}

bool
gimple_simplify_597 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1579;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 780, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1579:;
  return false;
}

bool
gimple_simplify_600 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1582;
  {
    tree tem;
    tem = captures[3];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 795, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1582:;
  return false;
}

bool
gimple_simplify_606 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[4]);
      if (element_precision (type) == element_precision (op_type)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1588;
	  {
	    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	    {
	      tree _o1[5], _r1;
	      _o1[0] = captures[0];
	      _o1[1] = captures[1];
	      _o1[2] = captures[2];
	      _o1[3] = captures[3];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[5];
		gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1588;
		_o1[4] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), cond_op, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1588;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 840, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1588:;
	}
  }
  return false;
}

bool
gimple_simplify_613 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[1])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1595;
  {
    res_op->set_op (cond_op, type, 5);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[2];
      _o1[1] = captures[0];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1595;
      res_op->ops[0] = _r1;
    }
    res_op->ops[1] = captures[3];
    res_op->ops[2] = captures[4];
    res_op->ops[3] = captures[5];
    res_op->ops[4] = captures[6];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 847, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1595:;
  return false;
}

bool
gimple_simplify_620 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1602;
	  {
	    res_op->set_op (MINUS_EXPR, type, 2);
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[1];
	      gimple_match_op tem_op (res_op->cond.any_else (), logs, TREE_TYPE (_o1[0]), _o1[0]);
	      tem_op.resimplify (NULL, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, NULL);
	      if (!_r1) goto next_after_fail1602;
	      res_op->ops[0] = _r1;
	    }
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      gimple_match_op tem_op (res_op->cond.any_else (), logs, TREE_TYPE (_o1[0]), _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1602;
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 854, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1602:;
	}
    }
  return false;
}

bool
gimple_simplify_632 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (froms),
 const combined_fn ARG_UNUSED (tos))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (optimize && canonicalize_math_p ()
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1614;
      {
	res_op->set_op (tos, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 866, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1614:;
    }
  return false;
}

bool
gimple_simplify_638 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (rints))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_p ()
)
    {
      if (!flag_errno_math
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1620;
	  {
	    res_op->set_op (FIX_TRUNC_EXPR, type, 1);
	    res_op->ops[0] = captures[0];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 872, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1620:;
	}
    }
  return false;
}

bool
gimple_simplify_645 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1627;
      {
	res_op->set_op (CFN_FNMA, type, 3);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[1];
	res_op->ops[2] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 879, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1627:;
    }
  return false;
}

bool
gimple_simplify_653 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree t = TREE_TYPE (captures[0]);
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1635;
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
	      if (!_r1) goto next_after_fail1635;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 884, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1635:;
  }
  return false;
}

bool
gimple_simplify_661 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (PARITY))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1648;
  {
    res_op->set_op (PARITY, type, 1);
    res_op->ops[0] = captures[0];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 895, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1648:;
  return false;
}

bool
gimple_simplify_662 (gimple_match_op *res_op, gimple_seq *seq,
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
	      && (!single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1649;
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
		    if (!_r2) goto next_after_fail1649;
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
		  if (!_r1) goto next_after_fail1649;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 896, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1649:;
      }
    }
  return false;
}

bool
gimple_simplify_669 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (uncond_op),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[1]);
      if (vectorized_internal_fn_supported_p (as_internal_fn (cond_op), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[1])
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1660;
	  {
	    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	    {
	      tree _o1[7], _r1;
	      _o1[0] = captures[0];
	      _o1[1] = captures[2];
	      _o1[2] = captures[3];
	      _o1[3] = captures[4];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[5];
		gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1660;
		_o1[4] = _r2;
	      }
	      _o1[5] = captures[6];
	      _o1[6] = captures[7];
	      gimple_match_op tem_op (res_op->cond.any_else (), cond_op, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5], _o1[6]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1660;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 905, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1660:;
	}
  }
  return false;
}

bool
gimple_simplify_CONVERT_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
				    if (INTEGRAL_TYPE_P (type)
 && tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
 && TYPE_PRECISION (type) > TYPE_PRECISION (TREE_TYPE (captures[2]))
 && TREE_CODE (type) != BOOLEAN_TYPE
)
				      {
					gimple_seq *lseq = seq;
					if (lseq
					    && (!single_use (captures[0])
					        || !single_use (captures[1])))
					  lseq = NULL;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1671;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[1], _r1;
					    _o1[0] = captures[2];
					    if (type != TREE_TYPE (_o1[0])
					        && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					      {
						gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
						tem_op.resimplify (lseq, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r1) goto next_after_fail1671;
					      }
					    else
					      _r1 = _o1[0];
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 916, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail1671:;
				      }
				  }
			          break;
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
		  {
		    tree _q20_pops[1];
		    if (gimple_nop_convert (_q20, _q20_pops, valueize))
		      {
			tree _q30 = _q20_pops[0];
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21 };
			  if (gimple_simplify_397 (res_op, seq, valueize, type, captures, BIT_AND_EXPR))
			    return true;
			}
		      }
		  }
		  {
		    tree _q21_pops[1];
		    if (gimple_nop_convert (_q21, _q21_pops, valueize))
		      {
			tree _q40 = _q21_pops[0];
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q20 };
			  if (gimple_simplify_397 (res_op, seq, valueize, type, captures, BIT_AND_EXPR))
			    return true;
			}
		      }
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
		  {
		    tree _q20_pops[1];
		    if (gimple_nop_convert (_q20, _q20_pops, valueize))
		      {
			tree _q30 = _q20_pops[0];
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21 };
			  if (gimple_simplify_397 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR))
			    return true;
			}
		      }
		  }
		  {
		    tree _q21_pops[1];
		    if (gimple_nop_convert (_q21, _q21_pops, valueize))
		      {
			tree _q40 = _q21_pops[0];
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q20 };
			  if (gimple_simplify_397 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR))
			    return true;
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
		  {
		    tree _q20_pops[1];
		    if (gimple_nop_convert (_q20, _q20_pops, valueize))
		      {
			tree _q30 = _q20_pops[0];
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21 };
			  if (gimple_simplify_397 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR))
			    return true;
			}
		      }
		  }
		  {
		    tree _q21_pops[1];
		    if (gimple_nop_convert (_q21, _q21_pops, valueize))
		      {
			tree _q40 = _q21_pops[0];
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q20 };
			  if (gimple_simplify_397 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR))
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
		  if (gimple_zero_one_valued_p (_q20, valueize))
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
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1672;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[1];
				      if (type != TREE_TYPE (_o1[0])
				          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					{
					  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail1672;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[0] = _r1;
				    }
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[2];
				      if (type != TREE_TYPE (_o1[0])
				          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					{
					  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail1672;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 917, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1672:;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q21))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			  if (POINTER_TYPE_P (type)
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1673;
			      {
				tree tem;
				tem =  build_fold_addr_expr_with_type
 (build2 (MEM_REF, char_type_node, captures[0],
 wide_int_to_tree (ptr_type_node, wi::neg (wi::to_wide (captures[1])))),
 type);
				res_op->set_value (tem);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 918, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1673:;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (gimple_zero_one_valued_p (_q20, valueize))
		    {
		      switch (TREE_CODE (_q21))
		        {
			case INTEGER_CST:
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      if (gimple_simplify_398 (res_op, seq, valueize, type, captures, EQ_EXPR))
			        return true;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
		  if (gimple_zero_one_valued_p (_q20, valueize))
		    {
		      switch (TREE_CODE (_q21))
		        {
			case INTEGER_CST:
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      if (gimple_simplify_398 (res_op, seq, valueize, type, captures, NE_EXPR))
			        return true;
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
				  switch (TREE_CODE (_q21))
				    {
				    case INTEGER_CST:
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q21 };
					  if (gimple_simplify_399 (res_op, seq, valueize, type, captures, MIN_EXPR))
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
	      case MAX_EXPR:
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
				  switch (TREE_CODE (_q21))
				    {
				    case INTEGER_CST:
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q21 };
					  if (gimple_simplify_399 (res_op, seq, valueize, type, captures, MAX_EXPR))
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
				  gimple_seq *lseq = seq;
				  if (lseq
				      && (!single_use (captures[0])))
				    lseq = NULL;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1674;
				  {
				    res_op->set_op (LSHIFT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[1];
				      if (type != TREE_TYPE (_o1[0])
				          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					{
					  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail1674;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[2];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 919, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1674:;
				}
			      else
				{
				  if (wi::ltu_p (wi::to_wide (captures[2]), TYPE_PRECISION (TREE_TYPE (captures[0])))
)
				    {
				      gimple_seq *lseq = seq;
				      if (lseq
				          && (!single_use (captures[0])))
				        lseq = NULL;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1675;
				      {
					tree tem;
					tem =  build_zero_cst (type);
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 920, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1675:;
				    }
				}
			    }
			}
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
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1676;
	{
	  tree tem;
	  tem = captures[0];
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 921, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail1676:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1677;
			    {
			      res_op->set_op (NOP_EXPR, type, 1);
			      res_op->ops[0] = captures[1];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 922, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail1677:;
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1678;
				{
				  res_op->set_op (NOP_EXPR, type, 1);
				  res_op->ops[0] = captures[1];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 923, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail1678:;
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
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1679;
				    {
				      res_op->set_op (NOP_EXPR, type, 1);
				      res_op->ops[0] = captures[1];
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 924, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail1679:;
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1680;
					{
					  res_op->set_op (NOP_EXPR, type, 1);
					  res_op->ops[0] = captures[1];
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 925, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail1680:;
				      }
				    else
				      {
					if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1681;
					    {
					      res_op->set_op (NOP_EXPR, type, 1);
					      res_op->ops[0] = captures[1];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 926, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail1681:;
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
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1682;
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
						    if (!_r1) goto next_after_fail1682;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 927, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail1682:;
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
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1683;
						    {
						      res_op->set_op (NOP_EXPR, type, 1);
						      res_op->ops[0] = captures[1];
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 928, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail1683:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1684;
			    {
			      res_op->set_op (NOP_EXPR, type, 1);
			      res_op->ops[0] = captures[1];
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 922, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail1684:;
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1685;
				{
				  res_op->set_op (NOP_EXPR, type, 1);
				  res_op->ops[0] = captures[1];
				  res_op->resimplify (lseq, valueize);
				  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 923, __FILE__, __LINE__, true);
				  return true;
				}
next_after_fail1685:;
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
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1686;
				    {
				      res_op->set_op (NOP_EXPR, type, 1);
				      res_op->ops[0] = captures[1];
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 924, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail1686:;
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1687;
					{
					  res_op->set_op (NOP_EXPR, type, 1);
					  res_op->ops[0] = captures[1];
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 925, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail1687:;
				      }
				    else
				      {
					if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1688;
					    {
					      res_op->set_op (NOP_EXPR, type, 1);
					      res_op->ops[0] = captures[1];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 926, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail1688:;
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
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1689;
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
						    if (!_r1) goto next_after_fail1689;
						    res_op->ops[0] = _r1;
						  }
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 927, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail1689:;
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
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1690;
						    {
						      res_op->set_op (NOP_EXPR, type, 1);
						      res_op->ops[0] = captures[1];
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 928, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail1690:;
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
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			  if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (captures[0]))
 && operand_equal_p (captures[1], build_low_bits_mask (TREE_TYPE (captures[1]),
 TYPE_PRECISION (type)), 0)
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1691;
			      {
				res_op->set_op (NOP_EXPR, type, 1);
				res_op->ops[0] = captures[0];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 929, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1691:;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  tree _q22 = gimple_assign_rhs3 (_a1);
		  _q22 = do_valueize (valueize, _q22);
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		    if (VECTOR_TYPE_P (type)
 && types_match (TREE_TYPE (captures[1]), truth_type_for (type))
)
		      {
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[0])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1692;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[1], _r1;
			    _o1[0] = captures[2];
			    if (type != TREE_TYPE (_o1[0])
			        && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
			      {
				gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
				tem_op.resimplify (NULL, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, NULL);
				if (!_r1) goto next_after_fail1692;
			      }
			    else
			      _r1 = _o1[0];
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[1], _r1;
			    _o1[0] = captures[3];
			    if (type != TREE_TYPE (_o1[0])
			        && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
			      {
				gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
				tem_op.resimplify (NULL, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, NULL);
				if (!_r1) goto next_after_fail1692;
			      }
			    else
			      _r1 = _o1[0];
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 930, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail1692:;
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
				    gimple_seq *lseq = seq;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1693;
				    {
				      res_op->set_op (COND_EXPR, type, 3);
				      res_op->ops[0] = captures[1];
				      res_op->ops[0] = unshare_expr (res_op->ops[0]);
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[2];
					if (type != TREE_TYPE (_o1[0])
					    && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					  {
					    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail1693;
					  }
					else
					  _r1 = _o1[0];
					res_op->ops[1] = _r1;
				      }
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[3];
					if (type != TREE_TYPE (_o1[0])
					    && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					  {
					    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail1693;
					  }
					else
					  _r1 = _o1[0];
					res_op->ops[2] = _r1;
				      }
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 931, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail1693:;
				  }
			      }
			      break;
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
				  switch (TREE_CODE (_q21))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q21))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  {
						    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						    if (gimple_simplify_403 (res_op, seq, valueize, type, captures, MAX_EXPR))
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
				  switch (TREE_CODE (_q21))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q21))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  {
						    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						    if (gimple_simplify_403 (res_op, seq, valueize, type, captures, MIN_EXPR))
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
				  switch (TREE_CODE (_q21))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q21))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  {
						    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50 };
						    if (gimple_simplify_404 (res_op, seq, valueize, type, captures, PLUS_EXPR))
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
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q21 };
				    if (gimple_simplify_405 (res_op, seq, valueize, type, captures, PLUS_EXPR))
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
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q40 };
				    if (gimple_simplify_406 (res_op, seq, valueize, type, captures, PLUS_EXPR))
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
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q21 };
		    if (gimple_simplify_407 (res_op, seq, valueize, type, captures, PLUS_EXPR))
		      return true;
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
				  switch (TREE_CODE (_q21))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q21))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  {
						    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50 };
						    if (gimple_simplify_404 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q21 };
				    if (gimple_simplify_405 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q40 };
				    if (gimple_simplify_406 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q21 };
		    if (gimple_simplify_407 (res_op, seq, valueize, type, captures, MINUS_EXPR))
		      return true;
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
			      CASE_CONVERT:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  switch (TREE_CODE (_q21))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q21))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  {
						    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50 };
						    if (gimple_simplify_404 (res_op, seq, valueize, type, captures, MULT_EXPR))
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
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q21 };
				    if (gimple_simplify_405 (res_op, seq, valueize, type, captures, MULT_EXPR))
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
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q40 };
				    if (gimple_simplify_406 (res_op, seq, valueize, type, captures, MULT_EXPR))
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
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q21 };
		    if (gimple_simplify_407 (res_op, seq, valueize, type, captures, MULT_EXPR))
		      return true;
		  }
	          break;
	        }
	      case RDIV_EXPR:
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
			      CASE_CONVERT:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  switch (TREE_CODE (_q21))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q21))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      CASE_CONVERT:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  {
						    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50 };
						    if (gimple_simplify_404 (res_op, seq, valueize, type, captures, RDIV_EXPR))
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
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q21 };
				    if (gimple_simplify_405 (res_op, seq, valueize, type, captures, RDIV_EXPR))
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
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q40 };
				    if (gimple_simplify_406 (res_op, seq, valueize, type, captures, RDIV_EXPR))
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
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q21 };
		    if (gimple_simplify_407 (res_op, seq, valueize, type, captures, RDIV_EXPR))
		      return true;
		  }
	          break;
	        }
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    if (gimple_simplify_408 (res_op, seq, valueize, type, captures))
		      return true;
		  }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_RINTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTF, CFN_RINT))
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
	      case CFN_BUILT_IN_RINTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTL, CFN_RINT))
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
	      case CFN_BUILT_IN_ROUND:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUND, CFN_ROUND))
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
	      case CFN_BUILT_IN_SQRTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, CFN_SQRT))
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
	      case CFN_BUILT_IN_SQRTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, CFN_SQRT))
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
	      case CFN_BUILT_IN_TRUNC:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNC, CFN_TRUNC))
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
	      case CFN_BUILT_IN_FMA:
	        if (gimple_call_num_args (_c1) == 3)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    tree _q22 = gimple_call_arg (_c1, 2);
		    _q22 = do_valueize (valueize, _q22);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    switch (TREE_CODE (_q22))
						      {
						      case SSA_NAME:
						        if (gimple *_d4 = get_def (valueize, _q22))
						          {
							    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							      switch (gimple_assign_rhs_code (_a4))
								{
								CASE_CONVERT:
								  {
								    tree _q70 = gimple_assign_rhs1 (_a4);
								    _q70 = do_valueize (valueize, _q70);
								    {
								      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q50, _q70 };
								      if (gimple_simplify_402 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMA, CFN_FMA))
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
	      case CFN_BUILT_IN_ROUNDF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDF, CFN_ROUND))
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
	      case CFN_BUILT_IN_ROUNDL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDL, CFN_ROUND))
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
	      case CFN_BUILT_IN_CEIL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEIL, CFN_CEIL))
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
	      case CFN_BUILT_IN_FMAF:
	        if (gimple_call_num_args (_c1) == 3)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    tree _q22 = gimple_call_arg (_c1, 2);
		    _q22 = do_valueize (valueize, _q22);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    switch (TREE_CODE (_q22))
						      {
						      case SSA_NAME:
						        if (gimple *_d4 = get_def (valueize, _q22))
						          {
							    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							      switch (gimple_assign_rhs_code (_a4))
								{
								CASE_CONVERT:
								  {
								    tree _q70 = gimple_assign_rhs1 (_a4);
								    _q70 = do_valueize (valueize, _q70);
								    {
								      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q50, _q70 };
								      if (gimple_simplify_402 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAF, CFN_FMA))
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
	      case CFN_BUILT_IN_FMAL:
	        if (gimple_call_num_args (_c1) == 3)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    tree _q22 = gimple_call_arg (_c1, 2);
		    _q22 = do_valueize (valueize, _q22);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    switch (TREE_CODE (_q22))
						      {
						      case SSA_NAME:
						        if (gimple *_d4 = get_def (valueize, _q22))
						          {
							    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							      switch (gimple_assign_rhs_code (_a4))
								{
								CASE_CONVERT:
								  {
								    tree _q70 = gimple_assign_rhs1 (_a4);
								    _q70 = do_valueize (valueize, _q70);
								    {
								      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q50, _q70 };
								      if (gimple_simplify_402 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAL, CFN_FMA))
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
	      case CFN_BUILT_IN_RINT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINT, CFN_RINT))
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
	      case CFN_BUILT_IN_SQRT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, CFN_SQRT))
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
	      case CFN_BUILT_IN_COPYSIGNF16:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF16))
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
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNF32:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32))
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
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNF64:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64))
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
	          }
	        break;
	      case CFN_BUILT_IN_FLOORF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF, CFN_FLOOR))
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
	      case CFN_BUILT_IN_FLOORL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORL, CFN_FLOOR))
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
	      case CFN_BUILT_IN_NEARBYINT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINT, CFN_NEARBYINT))
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
	      case CFN_BUILT_IN_NEARBYINTF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINTF, CFN_NEARBYINT))
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
	      case CFN_BUILT_IN_NEARBYINTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINTL, CFN_NEARBYINT))
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
	      case CFN_BUILT_IN_ROUNDEVEN:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDEVEN, CFN_ROUNDEVEN))
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
	      case CFN_BUILT_IN_COPYSIGNF128X:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128X))
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
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNF128:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF128))
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
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNF32X:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF32X))
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
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNF64X:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF64X))
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
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN))
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
	          }
	        break;
	      case CFN_BUILT_IN_ROUNDEVENF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDEVENF, CFN_ROUNDEVEN))
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
	      case CFN_BUILT_IN_ROUNDEVENL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDEVENL, CFN_ROUNDEVEN))
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
	      case CFN_BUILT_IN_TRUNCF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCF, CFN_TRUNC))
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
	      case CFN_BUILT_IN_TRUNCL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCL, CFN_TRUNC))
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
	      case CFN_BUILT_IN_COPYSIGNF:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF))
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
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNL:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL))
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
	          }
	        break;
	      case CFN_BUILT_IN_CEILF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILF, CFN_CEIL))
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
	      case CFN_BUILT_IN_CEILL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILL, CFN_CEIL))
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
	      case CFN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
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
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					      switch (gimple_assign_rhs_code (_a3))
						{
						CASE_CONVERT:
						  {
						    tree _q50 = gimple_assign_rhs1 (_a3);
						    _q50 = do_valueize (valueize, _q50);
						    {
						      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q50 };
						      if (gimple_simplify_401 (res_op, seq, valueize, type, captures, CFN_COPYSIGN))
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
	          }
	        break;
	      case CFN_BUILT_IN_FLOOR:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
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
				    if (gimple_float16_value_p (_q30, valueize))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					  if (gimple_simplify_400 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOOR, CFN_FLOOR))
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
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_PARITYLL (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_661 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_PARITYLL))
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
				    if (gimple_simplify_663 (res_op, seq, valueize, type, captures, LROTATE_EXPR, CFN_BUILT_IN_PARITYLL))
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
				    if (gimple_simplify_663 (res_op, seq, valueize, type, captures, RROTATE_EXPR, CFN_BUILT_IN_PARITYLL))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128, CFN_BUILT_IN_PARITYLL))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16, CFN_BUILT_IN_PARITYLL))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32, CFN_BUILT_IN_PARITYLL))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64, CFN_BUILT_IN_PARITYLL))
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
		    if (gimple_simplify_664 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_PARITYLL))
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
		    if (gimple_simplify_666 (res_op, seq, valueize, type, captures, LROTATE_EXPR, CFN_BUILT_IN_PARITYLL))
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
		    if (gimple_simplify_666 (res_op, seq, valueize, type, captures, RROTATE_EXPR, CFN_BUILT_IN_PARITYLL))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128, CFN_BUILT_IN_PARITYLL))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16, CFN_BUILT_IN_PARITYLL))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32, CFN_BUILT_IN_PARITYLL))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64, CFN_BUILT_IN_PARITYLL))
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
    if (gimple_simplify_660 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_PARITYLL))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_REDUC_FMAX (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_FMAX:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    switch (TREE_CODE (_q21))
		      {
		      case VECTOR_CST:
		        {
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			    gimple_seq *lseq = seq;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1969;
			    {
			      res_op->set_op (CFN_FMAX, type, 2);
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[0];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_FMAX, type, _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail1969;
				res_op->ops[0] = _r1;
			      }
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[1];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_FMAX, type, _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail1969;
				res_op->ops[1] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 991, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail1969:;
			  }
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
gimple_simplify (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper code, const tree type, tree _p0)
{
  switch (code.get_rep())
    {
    case ABS_EXPR:
      return gimple_simplify_ABS_EXPR (res_op, seq, valueize, code, type, _p0);
    CASE_CONVERT:
      return gimple_simplify_CONVERT_EXPR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_COSF:
      return gimple_simplify_CFN_BUILT_IN_COSF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_COS:
      return gimple_simplify_CFN_BUILT_IN_COS (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_COSL:
      return gimple_simplify_CFN_BUILT_IN_COSL (res_op, seq, valueize, code, type, _p0);
    case -CFN_COS:
      return gimple_simplify_CFN_COS (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_COSHF:
      return gimple_simplify_CFN_BUILT_IN_COSHF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_COSH:
      return gimple_simplify_CFN_BUILT_IN_COSH (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_COSHL:
      return gimple_simplify_CFN_BUILT_IN_COSHL (res_op, seq, valueize, code, type, _p0);
    case -CFN_COSH:
      return gimple_simplify_CFN_COSH (res_op, seq, valueize, code, type, _p0);
    case NEGATE_EXPR:
      return gimple_simplify_NEGATE_EXPR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CCOSF:
      return gimple_simplify_CFN_BUILT_IN_CCOSF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CCOS:
      return gimple_simplify_CFN_BUILT_IN_CCOS (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CCOSL:
      return gimple_simplify_CFN_BUILT_IN_CCOSL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CCOSHF:
      return gimple_simplify_CFN_BUILT_IN_CCOSHF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CCOSH:
      return gimple_simplify_CFN_BUILT_IN_CCOSH (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CCOSHL:
      return gimple_simplify_CFN_BUILT_IN_CCOSHL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CABSF:
      return gimple_simplify_CFN_BUILT_IN_CABSF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CABS:
      return gimple_simplify_CFN_BUILT_IN_CABS (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CABSL:
      return gimple_simplify_CFN_BUILT_IN_CABSL (res_op, seq, valueize, code, type, _p0);
    case BIT_NOT_EXPR:
      return gimple_simplify_BIT_NOT_EXPR (res_op, seq, valueize, code, type, _p0);
    case ABSU_EXPR:
      return gimple_simplify_ABSU_EXPR (res_op, seq, valueize, code, type, _p0);
    case VIEW_CONVERT_EXPR:
      return gimple_simplify_VIEW_CONVERT_EXPR (res_op, seq, valueize, code, type, _p0);
    case FLOAT_EXPR:
      return gimple_simplify_FLOAT_EXPR (res_op, seq, valueize, code, type, _p0);
    case FIX_TRUNC_EXPR:
      return gimple_simplify_FIX_TRUNC_EXPR (res_op, seq, valueize, code, type, _p0);
    case PAREN_EXPR:
      return gimple_simplify_PAREN_EXPR (res_op, seq, valueize, code, type, _p0);
    case REALPART_EXPR:
      return gimple_simplify_REALPART_EXPR (res_op, seq, valueize, code, type, _p0);
    case IMAGPART_EXPR:
      return gimple_simplify_IMAGPART_EXPR (res_op, seq, valueize, code, type, _p0);
    case CONJ_EXPR:
      return gimple_simplify_CONJ_EXPR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_BSWAP16:
      return gimple_simplify_CFN_BUILT_IN_BSWAP16 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_BSWAP32:
      return gimple_simplify_CFN_BUILT_IN_BSWAP32 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_BSWAP64:
      return gimple_simplify_CFN_BUILT_IN_BSWAP64 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_BSWAP128:
      return gimple_simplify_CFN_BUILT_IN_BSWAP128 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LOGF:
      return gimple_simplify_CFN_BUILT_IN_LOGF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LOG:
      return gimple_simplify_CFN_BUILT_IN_LOG (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LOGL:
      return gimple_simplify_CFN_BUILT_IN_LOGL (res_op, seq, valueize, code, type, _p0);
    case -CFN_LOG:
      return gimple_simplify_CFN_LOG (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LOG2F:
      return gimple_simplify_CFN_BUILT_IN_LOG2F (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LOG2:
      return gimple_simplify_CFN_BUILT_IN_LOG2 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LOG2L:
      return gimple_simplify_CFN_BUILT_IN_LOG2L (res_op, seq, valueize, code, type, _p0);
    case -CFN_LOG2:
      return gimple_simplify_CFN_LOG2 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LOG10F:
      return gimple_simplify_CFN_BUILT_IN_LOG10F (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LOG10:
      return gimple_simplify_CFN_BUILT_IN_LOG10 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LOG10L:
      return gimple_simplify_CFN_BUILT_IN_LOG10L (res_op, seq, valueize, code, type, _p0);
    case -CFN_LOG10:
      return gimple_simplify_CFN_LOG10 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_EXPF:
      return gimple_simplify_CFN_BUILT_IN_EXPF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_EXP:
      return gimple_simplify_CFN_BUILT_IN_EXP (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_EXPL:
      return gimple_simplify_CFN_BUILT_IN_EXPL (res_op, seq, valueize, code, type, _p0);
    case -CFN_EXP:
      return gimple_simplify_CFN_EXP (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_EXP2F:
      return gimple_simplify_CFN_BUILT_IN_EXP2F (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_EXP2:
      return gimple_simplify_CFN_BUILT_IN_EXP2 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_EXP2L:
      return gimple_simplify_CFN_BUILT_IN_EXP2L (res_op, seq, valueize, code, type, _p0);
    case -CFN_EXP2:
      return gimple_simplify_CFN_EXP2 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_EXP10F:
      return gimple_simplify_CFN_BUILT_IN_EXP10F (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_EXP10:
      return gimple_simplify_CFN_BUILT_IN_EXP10 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_EXP10L:
      return gimple_simplify_CFN_BUILT_IN_EXP10L (res_op, seq, valueize, code, type, _p0);
    case -CFN_EXP10:
      return gimple_simplify_CFN_EXP10 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_POW10F:
      return gimple_simplify_CFN_BUILT_IN_POW10F (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_POW10:
      return gimple_simplify_CFN_BUILT_IN_POW10 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_POW10L:
      return gimple_simplify_CFN_BUILT_IN_POW10L (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SQRTF:
      return gimple_simplify_CFN_BUILT_IN_SQRTF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SQRT:
      return gimple_simplify_CFN_BUILT_IN_SQRT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SQRTL:
      return gimple_simplify_CFN_BUILT_IN_SQRTL (res_op, seq, valueize, code, type, _p0);
    case -CFN_SQRT:
      return gimple_simplify_CFN_SQRT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CBRTF:
      return gimple_simplify_CFN_BUILT_IN_CBRTF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CBRT:
      return gimple_simplify_CFN_BUILT_IN_CBRT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CBRTL:
      return gimple_simplify_CFN_BUILT_IN_CBRTL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TANF:
      return gimple_simplify_CFN_BUILT_IN_TANF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TAN:
      return gimple_simplify_CFN_BUILT_IN_TAN (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TANL:
      return gimple_simplify_CFN_BUILT_IN_TANL (res_op, seq, valueize, code, type, _p0);
    case -CFN_TAN:
      return gimple_simplify_CFN_TAN (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SINF:
      return gimple_simplify_CFN_BUILT_IN_SINF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SIN:
      return gimple_simplify_CFN_BUILT_IN_SIN (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SINL:
      return gimple_simplify_CFN_BUILT_IN_SINL (res_op, seq, valueize, code, type, _p0);
    case -CFN_SIN:
      return gimple_simplify_CFN_SIN (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SINHF:
      return gimple_simplify_CFN_BUILT_IN_SINHF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SINH:
      return gimple_simplify_CFN_BUILT_IN_SINH (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SINHL:
      return gimple_simplify_CFN_BUILT_IN_SINHL (res_op, seq, valueize, code, type, _p0);
    case -CFN_SINH:
      return gimple_simplify_CFN_SINH (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNCF:
      return gimple_simplify_CFN_BUILT_IN_TRUNCF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNC:
      return gimple_simplify_CFN_BUILT_IN_TRUNC (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNCL:
      return gimple_simplify_CFN_BUILT_IN_TRUNCL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNCF16:
      return gimple_simplify_CFN_BUILT_IN_TRUNCF16 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNCF32:
      return gimple_simplify_CFN_BUILT_IN_TRUNCF32 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNCF64:
      return gimple_simplify_CFN_BUILT_IN_TRUNCF64 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNCF128:
      return gimple_simplify_CFN_BUILT_IN_TRUNCF128 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNCF32X:
      return gimple_simplify_CFN_BUILT_IN_TRUNCF32X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNCF64X:
      return gimple_simplify_CFN_BUILT_IN_TRUNCF64X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_TRUNCF128X:
      return gimple_simplify_CFN_BUILT_IN_TRUNCF128X (res_op, seq, valueize, code, type, _p0);
    case -CFN_TRUNC:
      return gimple_simplify_CFN_TRUNC (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOORF:
      return gimple_simplify_CFN_BUILT_IN_FLOORF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOOR:
      return gimple_simplify_CFN_BUILT_IN_FLOOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOORL:
      return gimple_simplify_CFN_BUILT_IN_FLOORL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOORF16:
      return gimple_simplify_CFN_BUILT_IN_FLOORF16 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOORF32:
      return gimple_simplify_CFN_BUILT_IN_FLOORF32 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOORF64:
      return gimple_simplify_CFN_BUILT_IN_FLOORF64 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOORF128:
      return gimple_simplify_CFN_BUILT_IN_FLOORF128 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOORF32X:
      return gimple_simplify_CFN_BUILT_IN_FLOORF32X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOORF64X:
      return gimple_simplify_CFN_BUILT_IN_FLOORF64X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FLOORF128X:
      return gimple_simplify_CFN_BUILT_IN_FLOORF128X (res_op, seq, valueize, code, type, _p0);
    case -CFN_FLOOR:
      return gimple_simplify_CFN_FLOOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEILF:
      return gimple_simplify_CFN_BUILT_IN_CEILF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEIL:
      return gimple_simplify_CFN_BUILT_IN_CEIL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEILL:
      return gimple_simplify_CFN_BUILT_IN_CEILL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEILF16:
      return gimple_simplify_CFN_BUILT_IN_CEILF16 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEILF32:
      return gimple_simplify_CFN_BUILT_IN_CEILF32 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEILF64:
      return gimple_simplify_CFN_BUILT_IN_CEILF64 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEILF128:
      return gimple_simplify_CFN_BUILT_IN_CEILF128 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEILF32X:
      return gimple_simplify_CFN_BUILT_IN_CEILF32X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEILF64X:
      return gimple_simplify_CFN_BUILT_IN_CEILF64X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEILF128X:
      return gimple_simplify_CFN_BUILT_IN_CEILF128X (res_op, seq, valueize, code, type, _p0);
    case -CFN_CEIL:
      return gimple_simplify_CFN_CEIL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUNDF:
      return gimple_simplify_CFN_BUILT_IN_ROUNDF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUND:
      return gimple_simplify_CFN_BUILT_IN_ROUND (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUNDL:
      return gimple_simplify_CFN_BUILT_IN_ROUNDL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUNDF16:
      return gimple_simplify_CFN_BUILT_IN_ROUNDF16 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUNDF32:
      return gimple_simplify_CFN_BUILT_IN_ROUNDF32 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUNDF64:
      return gimple_simplify_CFN_BUILT_IN_ROUNDF64 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUNDF128:
      return gimple_simplify_CFN_BUILT_IN_ROUNDF128 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUNDF32X:
      return gimple_simplify_CFN_BUILT_IN_ROUNDF32X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUNDF64X:
      return gimple_simplify_CFN_BUILT_IN_ROUNDF64X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ROUNDF128X:
      return gimple_simplify_CFN_BUILT_IN_ROUNDF128X (res_op, seq, valueize, code, type, _p0);
    case -CFN_ROUND:
      return gimple_simplify_CFN_ROUND (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINTF:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINTF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINT:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINTL:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINTL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINTF16:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINTF16 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINTF32:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINTF32 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINTF64:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINTF64 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINTF128:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINTF128 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINTF32X:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINTF32X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINTF64X:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINTF64X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_NEARBYINTF128X:
      return gimple_simplify_CFN_BUILT_IN_NEARBYINTF128X (res_op, seq, valueize, code, type, _p0);
    case -CFN_NEARBYINT:
      return gimple_simplify_CFN_NEARBYINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINTF:
      return gimple_simplify_CFN_BUILT_IN_RINTF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINT:
      return gimple_simplify_CFN_BUILT_IN_RINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINTL:
      return gimple_simplify_CFN_BUILT_IN_RINTL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINTF16:
      return gimple_simplify_CFN_BUILT_IN_RINTF16 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINTF32:
      return gimple_simplify_CFN_BUILT_IN_RINTF32 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINTF64:
      return gimple_simplify_CFN_BUILT_IN_RINTF64 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINTF128:
      return gimple_simplify_CFN_BUILT_IN_RINTF128 (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINTF32X:
      return gimple_simplify_CFN_BUILT_IN_RINTF32X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINTF64X:
      return gimple_simplify_CFN_BUILT_IN_RINTF64X (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_RINTF128X:
      return gimple_simplify_CFN_BUILT_IN_RINTF128X (res_op, seq, valueize, code, type, _p0);
    case -CFN_RINT:
      return gimple_simplify_CFN_RINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEXPF:
      return gimple_simplify_CFN_BUILT_IN_CEXPF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEXP:
      return gimple_simplify_CFN_BUILT_IN_CEXP (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CEXPL:
      return gimple_simplify_CFN_BUILT_IN_CEXPL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_IFLOORL:
      return gimple_simplify_CFN_BUILT_IN_IFLOORL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LFLOORL:
      return gimple_simplify_CFN_BUILT_IN_LFLOORL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLFLOORL:
      return gimple_simplify_CFN_BUILT_IN_LLFLOORL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ICEILL:
      return gimple_simplify_CFN_BUILT_IN_ICEILL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LCEILL:
      return gimple_simplify_CFN_BUILT_IN_LCEILL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLCEILL:
      return gimple_simplify_CFN_BUILT_IN_LLCEILL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_IROUNDL:
      return gimple_simplify_CFN_BUILT_IN_IROUNDL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LROUNDL:
      return gimple_simplify_CFN_BUILT_IN_LROUNDL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLROUNDL:
      return gimple_simplify_CFN_BUILT_IN_LLROUNDL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_IRINTL:
      return gimple_simplify_CFN_BUILT_IN_IRINTL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LRINTL:
      return gimple_simplify_CFN_BUILT_IN_LRINTL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLRINTL:
      return gimple_simplify_CFN_BUILT_IN_LLRINTL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_IFLOOR:
      return gimple_simplify_CFN_BUILT_IN_IFLOOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LFLOOR:
      return gimple_simplify_CFN_BUILT_IN_LFLOOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLFLOOR:
      return gimple_simplify_CFN_BUILT_IN_LLFLOOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ICEIL:
      return gimple_simplify_CFN_BUILT_IN_ICEIL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LCEIL:
      return gimple_simplify_CFN_BUILT_IN_LCEIL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLCEIL:
      return gimple_simplify_CFN_BUILT_IN_LLCEIL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_IROUND:
      return gimple_simplify_CFN_BUILT_IN_IROUND (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LROUND:
      return gimple_simplify_CFN_BUILT_IN_LROUND (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLROUND:
      return gimple_simplify_CFN_BUILT_IN_LLROUND (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_IRINT:
      return gimple_simplify_CFN_BUILT_IN_IRINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LRINT:
      return gimple_simplify_CFN_BUILT_IN_LRINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLRINT:
      return gimple_simplify_CFN_BUILT_IN_LLRINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_IFLOORF:
      return gimple_simplify_CFN_BUILT_IN_IFLOORF (res_op, seq, valueize, code, type, _p0);
    case -CFN_IFLOOR:
      return gimple_simplify_CFN_IFLOOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LFLOORF:
      return gimple_simplify_CFN_BUILT_IN_LFLOORF (res_op, seq, valueize, code, type, _p0);
    case -CFN_LFLOOR:
      return gimple_simplify_CFN_LFLOOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLFLOORF:
      return gimple_simplify_CFN_BUILT_IN_LLFLOORF (res_op, seq, valueize, code, type, _p0);
    case -CFN_LLFLOOR:
      return gimple_simplify_CFN_LLFLOOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_ICEILF:
      return gimple_simplify_CFN_BUILT_IN_ICEILF (res_op, seq, valueize, code, type, _p0);
    case -CFN_ICEIL:
      return gimple_simplify_CFN_ICEIL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LCEILF:
      return gimple_simplify_CFN_BUILT_IN_LCEILF (res_op, seq, valueize, code, type, _p0);
    case -CFN_LCEIL:
      return gimple_simplify_CFN_LCEIL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLCEILF:
      return gimple_simplify_CFN_BUILT_IN_LLCEILF (res_op, seq, valueize, code, type, _p0);
    case -CFN_LLCEIL:
      return gimple_simplify_CFN_LLCEIL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_IROUNDF:
      return gimple_simplify_CFN_BUILT_IN_IROUNDF (res_op, seq, valueize, code, type, _p0);
    case -CFN_IROUND:
      return gimple_simplify_CFN_IROUND (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LROUNDF:
      return gimple_simplify_CFN_BUILT_IN_LROUNDF (res_op, seq, valueize, code, type, _p0);
    case -CFN_LROUND:
      return gimple_simplify_CFN_LROUND (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLROUNDF:
      return gimple_simplify_CFN_BUILT_IN_LLROUNDF (res_op, seq, valueize, code, type, _p0);
    case -CFN_LLROUND:
      return gimple_simplify_CFN_LLROUND (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_IRINTF:
      return gimple_simplify_CFN_BUILT_IN_IRINTF (res_op, seq, valueize, code, type, _p0);
    case -CFN_IRINT:
      return gimple_simplify_CFN_IRINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LRINTF:
      return gimple_simplify_CFN_BUILT_IN_LRINTF (res_op, seq, valueize, code, type, _p0);
    case -CFN_LRINT:
      return gimple_simplify_CFN_LRINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_LLRINTF:
      return gimple_simplify_CFN_BUILT_IN_LLRINTF (res_op, seq, valueize, code, type, _p0);
    case -CFN_LLRINT:
      return gimple_simplify_CFN_LLRINT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CPROJF:
      return gimple_simplify_CFN_BUILT_IN_CPROJF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CPROJ:
      return gimple_simplify_CFN_BUILT_IN_CPROJ (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CPROJL:
      return gimple_simplify_CFN_BUILT_IN_CPROJL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SIGNBITF:
      return gimple_simplify_CFN_BUILT_IN_SIGNBITF (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SIGNBIT:
      return gimple_simplify_CFN_BUILT_IN_SIGNBIT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_SIGNBITL:
      return gimple_simplify_CFN_BUILT_IN_SIGNBITL (res_op, seq, valueize, code, type, _p0);
    case -CFN_SIGNBIT:
      return gimple_simplify_CFN_SIGNBIT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CTZ:
      return gimple_simplify_CFN_BUILT_IN_CTZ (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CTZL:
      return gimple_simplify_CFN_BUILT_IN_CTZL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CTZLL:
      return gimple_simplify_CFN_BUILT_IN_CTZLL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_CTZIMAX:
      return gimple_simplify_CFN_BUILT_IN_CTZIMAX (res_op, seq, valueize, code, type, _p0);
    case -CFN_CTZ:
      return gimple_simplify_CFN_CTZ (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_POPCOUNT:
      return gimple_simplify_CFN_BUILT_IN_POPCOUNT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_POPCOUNTL:
      return gimple_simplify_CFN_BUILT_IN_POPCOUNTL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_POPCOUNTLL:
      return gimple_simplify_CFN_BUILT_IN_POPCOUNTLL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_POPCOUNTIMAX:
      return gimple_simplify_CFN_BUILT_IN_POPCOUNTIMAX (res_op, seq, valueize, code, type, _p0);
    case -CFN_POPCOUNT:
      return gimple_simplify_CFN_POPCOUNT (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_PARITY:
      return gimple_simplify_CFN_BUILT_IN_PARITY (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_PARITYL:
      return gimple_simplify_CFN_BUILT_IN_PARITYL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_PARITYLL:
      return gimple_simplify_CFN_BUILT_IN_PARITYLL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_PARITYIMAX:
      return gimple_simplify_CFN_BUILT_IN_PARITYIMAX (res_op, seq, valueize, code, type, _p0);
    case -CFN_PARITY:
      return gimple_simplify_CFN_PARITY (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FFS:
      return gimple_simplify_CFN_BUILT_IN_FFS (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FFSL:
      return gimple_simplify_CFN_BUILT_IN_FFSL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FFSLL:
      return gimple_simplify_CFN_BUILT_IN_FFSLL (res_op, seq, valueize, code, type, _p0);
    case -CFN_BUILT_IN_FFSIMAX:
      return gimple_simplify_CFN_BUILT_IN_FFSIMAX (res_op, seq, valueize, code, type, _p0);
    case -CFN_FFS:
      return gimple_simplify_CFN_FFS (res_op, seq, valueize, code, type, _p0);
    case -CFN_REDUC_PLUS:
      return gimple_simplify_CFN_REDUC_PLUS (res_op, seq, valueize, code, type, _p0);
    case -CFN_REDUC_IOR:
      return gimple_simplify_CFN_REDUC_IOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_REDUC_XOR:
      return gimple_simplify_CFN_REDUC_XOR (res_op, seq, valueize, code, type, _p0);
    case -CFN_REDUC_MAX:
      return gimple_simplify_CFN_REDUC_MAX (res_op, seq, valueize, code, type, _p0);
    case -CFN_REDUC_MIN:
      return gimple_simplify_CFN_REDUC_MIN (res_op, seq, valueize, code, type, _p0);
    case -CFN_REDUC_FMAX:
      return gimple_simplify_CFN_REDUC_FMAX (res_op, seq, valueize, code, type, _p0);
    case -CFN_REDUC_FMIN:
      return gimple_simplify_CFN_REDUC_FMIN (res_op, seq, valueize, code, type, _p0);
    case -CFN_REDUC_AND:
      return gimple_simplify_CFN_REDUC_AND (res_op, seq, valueize, code, type, _p0);
    default:;
    }
  return false;
}

bool
gimple_simplify_NE_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_211 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_211 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			    if (gimple_simplify_212 (res_op, seq, valueize, type, captures, NE_EXPR, CEIL_MOD_EXPR))
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
			    if (gimple_simplify_212 (res_op, seq, valueize, type, captures, NE_EXPR, FLOOR_MOD_EXPR))
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
			    if (gimple_simplify_212 (res_op, seq, valueize, type, captures, NE_EXPR, ROUND_MOD_EXPR))
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
			    if (gimple_simplify_212 (res_op, seq, valueize, type, captures, NE_EXPR, TRUNC_MOD_EXPR))
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
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2278;
				      {
					res_op->set_op (NOP_EXPR, type, 1);
					res_op->ops[0] = captures[0];
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1088, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2278:;
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
					if (gimple_simplify_215 (res_op, seq, valueize, type, captures, NE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_215 (res_op, seq, valueize, type, captures, NE_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_215 (res_op, seq, valueize, type, captures, NE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_215 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					      if (gimple_simplify_216 (res_op, seq, valueize, type, captures, NE_EXPR))
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
									      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
					  return true;
				      }
				    }
				  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q41, _q20 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
					      return true;
					  }
				        }
				      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q51, _q20 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				if (gimple_simplify_220 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2279;
					    {
					      res_op->set_op (NE_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2279:;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2280;
					    {
					      res_op->set_op (NE_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2280:;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2281;
					    {
					      res_op->set_op (NE_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2281:;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2282;
					    {
					      res_op->set_op (NE_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2282:;
					  }
				      }
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
							if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20 };
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20 };
			if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_221 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							if (gimple_simplify_222 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_222 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_222 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_222 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2283;
					    {
					      res_op->set_op (NE_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1028, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2283:;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2284;
					    {
					      res_op->set_op (NE_EXPR, type, 2);
					      res_op->ops[0] = captures[2];
					      res_op->ops[1] = captures[1];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1029, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2284:;
					  }
				      }
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
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_224 (res_op, seq, valueize, type, captures, NE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
					if (gimple_simplify_225 (res_op, seq, valueize, type, captures, NE_EXPR))
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
		if (gimple_simplify_213 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
					      return true;
					  }
				        }
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30 };
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
			      return true;
			  }
		        }
		      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30 };
			    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, NE_EXPR))
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
    {
      tree _p1_pops[1];
      if (gimple_logical_inverted_value (_p1, _p1_pops, valueize))
        {
	  tree _q30 = _p1_pops[0];
	  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	    {
	      {
		tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		if (gimple_simplify_213 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
			  return true;
		      }
		    }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _p0 };
			if (gimple_simplify_219 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
			      return true;
			  }
		        }
		      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _p0 };
			    if (gimple_simplify_218 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							if (gimple_simplify_222 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_222 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_222 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_222 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			    if (gimple_simplify_226 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				    if (gimple_simplify_227 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				    if (gimple_simplify_227 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			  if (gimple_simplify_227 (res_op, seq, valueize, type, captures, NE_EXPR))
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
								    if (gimple_simplify_228 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							  if (gimple_simplify_228 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							  if (gimple_simplify_228 (res_op, seq, valueize, type, captures, NE_EXPR))
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
						if (gimple_simplify_228 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			  if (gimple_simplify_227 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_229 (res_op, seq, valueize, type, captures, NE_EXPR, GE_EXPR))
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
									      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
									      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			      if (gimple_simplify_230 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = GT_EXPR;
					    const enum tree_code rcmp = NE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2285;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2285:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2286;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2286:;
					      }
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = EQ_EXPR;
					    const enum tree_code rcmp = LE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2287;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2287:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2288;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2288:;
					      }
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = NE_EXPR;
					    const enum tree_code rcmp = GT_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2289;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2289:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2290;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2290:;
					      }
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = GT_EXPR;
					    const enum tree_code rcmp = NE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2291;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2291:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2292;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2292:;
					      }
					  }
				        }
				    }
			          break;
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = GE_EXPR;
					    const enum tree_code rcmp = NE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2293;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2293:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2294;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2294:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = EQ_EXPR;
					    const enum tree_code rcmp = LT_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2295;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2295:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2296;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2296:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = NE_EXPR;
					    const enum tree_code rcmp = GE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2297;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2297:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2298;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2298:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = GE_EXPR;
					    const enum tree_code rcmp = NE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2299;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2299:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2300;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2300:;
					      }
					  }
				        }
				    }
			          break;
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = GT_EXPR;
					    const enum tree_code rcmp = NE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2301;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2301:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2302;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2302:;
					      }
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = EQ_EXPR;
					    const enum tree_code rcmp = LE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2303;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2303:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2304;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2304:;
					      }
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = NE_EXPR;
					    const enum tree_code rcmp = GT_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2305;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2305:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2306;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2306:;
					      }
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = GT_EXPR;
					    const enum tree_code rcmp = NE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2307;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2307:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2308;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2308:;
					      }
					  }
				        }
				    }
			          break;
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = GE_EXPR;
					    const enum tree_code rcmp = NE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2309;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2309:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2310;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2310:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = EQ_EXPR;
					    const enum tree_code rcmp = LT_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2311;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2311:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2312;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2312:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = NE_EXPR;
					    const enum tree_code rcmp = GE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2313;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2313:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2314;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2314:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = GE_EXPR;
					    const enum tree_code rcmp = NE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2315;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2315:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2316;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2316:;
					      }
					  }
				        }
				    }
			          break;
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = EQ_EXPR;
					    const enum tree_code rcmp = LE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2317;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2317:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2318;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2318:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = EQ_EXPR;
					    const enum tree_code rcmp = LT_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2319;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2319:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2320;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2320:;
					      }
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = EQ_EXPR;
					    const enum tree_code rcmp = LE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2321;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2321:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2322;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2322:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = EQ_EXPR;
					    const enum tree_code rcmp = LT_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2323;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2323:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2324;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2324:;
					      }
					  }
				        }
				    }
			          break;
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = NE_EXPR;
					    const enum tree_code rcmp = GT_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2325;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2325:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2326;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2326:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = NE_EXPR;
					    const enum tree_code rcmp = GE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2327;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2327:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2328;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2328:;
					      }
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
					    const enum tree_code cmp1 = LT_EXPR;
					    const enum tree_code cmp2 = NE_EXPR;
					    const enum tree_code rcmp = GT_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2329;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2329:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2330;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2330:;
					      }
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
					    const enum tree_code cmp1 = LE_EXPR;
					    const enum tree_code cmp2 = NE_EXPR;
					    const enum tree_code rcmp = GE_EXPR;
					    if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2331;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2331:;
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2332;
						{
						  res_op->set_op (rcmp, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2332:;
					      }
					  }
				        }
				    }
			          break;
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, NE_EXPR, GT_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, NE_EXPR, GT_EXPR))
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
				if (gimple_simplify_234 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, NE_EXPR, LT_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, NE_EXPR, LT_EXPR))
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
				if (gimple_simplify_235 (res_op, seq, valueize, type, captures, NE_EXPR))
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
	      if (gimple_simplify_231 (res_op, seq, valueize, type, captures, NE_EXPR))
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
	      if (gimple_simplify_231 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, NE_EXPR, GT_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, NE_EXPR, GT_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, NE_EXPR, LT_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, NE_EXPR, LT_EXPR))
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
						if (gimple_simplify_236 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR))
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
						      if (gimple_simplify_237 (res_op, seq, valueize, type, captures, NE_EXPR))
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
						if (gimple_simplify_238 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR))
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
						if (gimple_simplify_239 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR))
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
						      if (gimple_simplify_240 (res_op, seq, valueize, type, captures, NE_EXPR))
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
						    if (gimple_simplify_238 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR))
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
			    if (gimple_simplify_241 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR))
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
				if (gimple_simplify_242 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_243 (res_op, seq, valueize, type, captures, LROTATE_EXPR, RROTATE_EXPR, NE_EXPR))
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
				if (gimple_simplify_244 (res_op, seq, valueize, type, captures, LROTATE_EXPR, RROTATE_EXPR, NE_EXPR))
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
			  if (gimple_simplify_245 (res_op, seq, valueize, type, captures, LROTATE_EXPR, RROTATE_EXPR, NE_EXPR))
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
					if (gimple_simplify_243 (res_op, seq, valueize, type, captures, RROTATE_EXPR, LROTATE_EXPR, NE_EXPR))
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
				if (gimple_simplify_244 (res_op, seq, valueize, type, captures, RROTATE_EXPR, LROTATE_EXPR, NE_EXPR))
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
			  if (gimple_simplify_245 (res_op, seq, valueize, type, captures, RROTATE_EXPR, LROTATE_EXPR, NE_EXPR))
			    return true;
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
 && (TREE_CODE_CLASS (NE_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2333;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2333;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2333;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2333:;
					  }
				      }
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
 && (TREE_CODE_CLASS (NE_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2334;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2334;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2334;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2334:;
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
					if (gimple_simplify_247 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_BSWAP128))
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
			    if (gimple_simplify_248 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_BSWAP128))
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
					if (gimple_simplify_247 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_BSWAP16))
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
			    if (gimple_simplify_248 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_BSWAP16))
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
					if (gimple_simplify_247 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_BSWAP32))
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
			    if (gimple_simplify_248 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_BSWAP32))
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
					if (gimple_simplify_247 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_BSWAP64))
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
			    if (gimple_simplify_248 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_BSWAP64))
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
 && (TREE_CODE_CLASS (NE_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2335;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2335;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2335;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2335:;
		      }
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
					    const enum tree_code eqne = NE_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2336;
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
						      if (!_r2) goto next_after_fail2336;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2336;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2336;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1082, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2336:;
					      }
					  }
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q21, _q22, _q20 };
					    const enum tree_code eqne = NE_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2337;
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
						      if (!_r2) goto next_after_fail2337;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2337;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2337;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1082, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2337:;
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
					    const enum tree_code eqne = NE_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2338;
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
						      if (!_r2) goto next_after_fail2338;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2338;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2338;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1083, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2338:;
					      }
					  }
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q22, _q21, _q20 };
					    const enum tree_code eqne = NE_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2339;
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
						      if (!_r2) goto next_after_fail2339;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2339;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2339;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1083, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2339:;
					      }
					  }
				        }
				    }
			          break;
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
					    const enum tree_code eqne = NE_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2340;
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
						      if (!_r2) goto next_after_fail2340;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2340;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2340;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1082, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2340:;
					      }
					  }
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q21, _q22, _q20 };
					    const enum tree_code eqne = NE_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2341;
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
						      if (!_r2) goto next_after_fail2341;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), NE_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2341;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2341;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1082, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2341:;
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
					    const enum tree_code eqne = NE_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2342;
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
						      if (!_r2) goto next_after_fail2342;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2342;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2342;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1083, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2342:;
					      }
					  }
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q22, _q21, _q20 };
					    const enum tree_code eqne = NE_EXPR;
					    if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2343;
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
						      if (!_r2) goto next_after_fail2343;
						      _o1[0] = _r2;
						    }
						    {
						      tree _o2[2], _r2;
						      _o2[0] = captures[1];
						      _o2[1] = captures[2];
						      gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, type, _o2[0], _o2[1]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2343;
						      _o1[1] = _r2;
						    }
						    gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2343;
						    res_op->ops[0] = _r1;
						  }
						  res_op->ops[1] =  constant_boolean_node (eqne != NE_EXPR, type);
						  res_op->ops[2] =  constant_boolean_node (eqne == NE_EXPR, type);
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1083, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2343:;
					      }
					  }
				        }
				    }
			          break;
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, LT_EXPR, GE_EXPR, UNGE_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, LT_EXPR, GE_EXPR, UNGE_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, LE_EXPR, GT_EXPR, UNGT_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, LE_EXPR, GT_EXPR, UNGT_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR, NE_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR, NE_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR, EQ_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR, EQ_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, GE_EXPR, LT_EXPR, UNLT_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, GE_EXPR, LT_EXPR, UNLT_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, GT_EXPR, LE_EXPR, UNLE_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, GT_EXPR, LE_EXPR, UNLE_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, UNORDERED_EXPR, ORDERED_EXPR, ORDERED_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, UNORDERED_EXPR, ORDERED_EXPR, ORDERED_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, ORDERED_EXPR, UNORDERED_EXPR, UNORDERED_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, ORDERED_EXPR, UNORDERED_EXPR, UNORDERED_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, UNLT_EXPR, GE_EXPR, GE_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, UNLT_EXPR, GE_EXPR, GE_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, UNLE_EXPR, GT_EXPR, GT_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, UNLE_EXPR, GT_EXPR, GT_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, UNGT_EXPR, LE_EXPR, LE_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, UNGT_EXPR, LE_EXPR, LE_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, UNGE_EXPR, LT_EXPR, LT_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, UNGE_EXPR, LT_EXPR, LT_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, UNEQ_EXPR, LTGT_EXPR, LTGT_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, UNEQ_EXPR, LTGT_EXPR, LTGT_EXPR))
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
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_304 (res_op, seq, valueize, type, captures, LTGT_EXPR, UNEQ_EXPR, UNEQ_EXPR))
			  return true;
		      }
		    }
		  if (integer_truep (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_305 (res_op, seq, valueize, type, captures, LTGT_EXPR, UNEQ_EXPR, UNEQ_EXPR))
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
			if (gimple_simplify_251 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_252 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				if (gimple_simplify_254 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			    if (gimple_simplify_253 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
	if (gimple_simplify_306 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_256 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_256 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				    if (gimple_simplify_257 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_257 (res_op, seq, valueize, type, captures, NE_EXPR))
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
		      if (gimple_simplify_258 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
					if (gimple_simplify_259 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_259 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			  if (gimple_simplify_258 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
	  if (gimple_simplify_260 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, PLUS_EXPR, NE_EXPR))
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, MINUS_EXPR, NE_EXPR))
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
				if (gimple_simplify_262 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
			  if (gimple_simplify_266 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
			  if (gimple_simplify_267 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
			const enum tree_code eqne = NE_EXPR;
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2344;
			{
			  res_op->set_op (eqne, type, 2);
			  res_op->ops[0] = captures[0];
			  res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[0]));
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1084, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2344:;
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
			const enum tree_code eqne = NE_EXPR;
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2345;
			{
			  res_op->set_op (eqne, type, 2);
			  res_op->ops[0] = captures[0];
			  res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[0]));
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1084, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2345:;
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
				    if (gimple_simplify_270 (res_op, seq, valueize, type, captures, NE_EXPR))
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
		    if (gimple_simplify_271 (res_op, seq, valueize, type, captures, NE_EXPR))
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
						if (gimple_simplify_272 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			  if (gimple_simplify_273 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				if (gimple_simplify_272 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_274 (res_op, seq, valueize, type, captures, NE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_275 (res_op, seq, valueize, type, captures, NE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_275 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, NE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, NE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, NE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, NE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, NE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, NE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_SQRT, NE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_SQRT, NE_EXPR))
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
		if (gimple_simplify_273 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_275 (res_op, seq, valueize, type, captures, NE_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_275 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_276 (res_op, seq, valueize, type, captures, NE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_276 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_276 (res_op, seq, valueize, type, captures, NE_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_276 (res_op, seq, valueize, type, captures, NE_EXPR))
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
									      if (gimple_simplify_277 (res_op, seq, valueize, type, captures, NE_EXPR))
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
									      if (gimple_simplify_277 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_278 (res_op, seq, valueize, type, captures, NE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_278 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			if (gimple_simplify_278 (res_op, seq, valueize, type, captures, NE_EXPR))
			  return true;
		      }
		    }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q31, _p0 };
			if (gimple_simplify_278 (res_op, seq, valueize, type, captures, NE_EXPR))
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
						if (gimple_simplify_279 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				if (gimple_simplify_279 (res_op, seq, valueize, type, captures, NE_EXPR))
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
	    if (gimple_simplify_280 (res_op, seq, valueize, type, captures, NE_EXPR))
	      return true;
	  }
        }
    }
}
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_280 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					if (gimple_simplify_281 (res_op, seq, valueize, type, captures, NE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
					if (gimple_simplify_281 (res_op, seq, valueize, type, captures, NE_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
					if (gimple_simplify_281 (res_op, seq, valueize, type, captures, NE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
					if (gimple_simplify_281 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			    if (gimple_simplify_282 (res_op, seq, valueize, type, captures, NE_EXPR, EQ_EXPR))
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
					    if (gimple_simplify_283 (res_op, seq, valueize, type, captures, GE_EXPR, NE_EXPR, LT_EXPR))
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
					    if (gimple_simplify_284 (res_op, seq, valueize, type, captures, NE_EXPR, GE_EXPR))
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
					    if (gimple_simplify_283 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, LT_EXPR))
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
					    if (gimple_simplify_285 (res_op, seq, valueize, type, captures, NE_EXPR, GE_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, NE_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					    if (gimple_simplify_287 (res_op, seq, valueize, type, captures, NE_EXPR, LT_EXPR))
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
			    if (gimple_simplify_288 (res_op, seq, valueize, type, captures, NE_EXPR, LT_EXPR))
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
								    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, NE_EXPR))
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
						    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, NE_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, NE_EXPR))
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
						if (gimple_simplify_289 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				if (gimple_simplify_289 (res_op, seq, valueize, type, captures, NE_EXPR))
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
						    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				if (gimple_simplify_290 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, NE_EXPR))
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
		if (gimple_simplify_290 (res_op, seq, valueize, type, captures, NE_EXPR))
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
    if (gimple_simplify_291 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			  if (gimple_simplify_292 (res_op, seq, valueize, type, captures, NE_EXPR))
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
					      if (gimple_simplify_293 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
					      if (gimple_simplify_293 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR))
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
					      if (gimple_simplify_293 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
					      if (gimple_simplify_293 (res_op, seq, valueize, type, captures, EQ_EXPR, NE_EXPR))
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
if (integer_zerop (_p1))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (TREE_CODE (TREE_TYPE (captures[0])) == BOOLEAN_TYPE
 && types_match (type, TREE_TYPE (captures[0]))
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2346;
	  {
	    tree tem;
	    tem = captures[0];
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1089, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail2346:;
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
			      const enum tree_code cmp = NE_EXPR;
			      const enum tree_code icmp = GT_EXPR;
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
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2347;
						  {
						    res_op->set_op (icmp, type, 2);
						    res_op->ops[0] = captures[1];
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1086, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2347:;
						}
					      else
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2348;
						  {
						    res_op->set_op (icmp, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2348;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1087, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2348:;
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
			      const enum tree_code cmp = NE_EXPR;
			      const enum tree_code icmp = GT_EXPR;
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
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2349;
						  {
						    res_op->set_op (icmp, type, 2);
						    res_op->ops[0] = captures[1];
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1086, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2349:;
						}
					      else
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2350;
						  {
						    res_op->set_op (icmp, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2350;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1087, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2350:;
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
			      const enum tree_code cmp = NE_EXPR;
			      const enum tree_code icmp = GT_EXPR;
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
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2351;
						  {
						    res_op->set_op (icmp, type, 2);
						    res_op->ops[0] = captures[1];
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1086, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2351:;
						}
					      else
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2352;
						  {
						    res_op->set_op (icmp, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2352;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1087, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2352:;
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
			      const enum tree_code cmp = NE_EXPR;
			      const enum tree_code icmp = GT_EXPR;
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
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2353;
						  {
						    res_op->set_op (icmp, type, 2);
						    res_op->ops[0] = captures[1];
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1086, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2353:;
						}
					      else
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2354;
						  {
						    res_op->set_op (icmp, type, 2);
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, utype, _o1[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r1) goto next_after_fail2354;
						      res_op->ops[0] = _r1;
						    }
						    res_op->ops[1] =  csts;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1087, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2354:;
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
									      if (gimple_simplify_294 (res_op, seq, valueize, type, captures, NE_EXPR))
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
				if (gimple_simplify_295 (res_op, seq, valueize, type, captures, PLUS_EXPR, MINUS_EXPR, NE_EXPR))
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
				if (gimple_simplify_295 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR, NE_EXPR))
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
				if (gimple_simplify_296 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR))
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, NE_EXPR, GE_EXPR, CFN_BUILT_IN_CLZ))
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_CTZ))
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_FFS))
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
			  if (gimple_simplify_303 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_FFSIMAX))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNT))
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, NE_EXPR, GE_EXPR, CFN_BUILT_IN_CLZL))
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_CTZL))
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_FFSL))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTLL))
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_CTZIMAX))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR, CFN_POPCOUNT))
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, NE_EXPR, GE_EXPR, CFN_CLZ))
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
			    if (gimple_simplify_298 (res_op, seq, valueize, type, captures, NE_EXPR, GE_EXPR))
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_CTZ))
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
			    if (gimple_simplify_300 (res_op, seq, valueize, type, captures, NE_EXPR))
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_FFS))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTIMAX))
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, NE_EXPR, GE_EXPR, CFN_BUILT_IN_CLZIMAX))
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
			  if (gimple_simplify_301 (res_op, seq, valueize, type, captures, NE_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTL))
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
			    if (gimple_simplify_297 (res_op, seq, valueize, type, captures, NE_EXPR, GE_EXPR, CFN_BUILT_IN_CLZLL))
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
			    if (gimple_simplify_299 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_CTZLL))
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
			    if (gimple_simplify_302 (res_op, seq, valueize, type, captures, NE_EXPR, CFN_BUILT_IN_FFSLL))
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
gimple_simplify_CEIL_MOD_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_329 (res_op, seq, valueize, type, captures, CEIL_MOD_EXPR))
	  return true;
      }
    }
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_330 (res_op, seq, valueize, type, captures, CEIL_MOD_EXPR))
	  return true;
      }
    }
  if (integer_minus_onep (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_331 (res_op, seq, valueize, type, captures, CEIL_MOD_EXPR))
	  return true;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_332 (res_op, seq, valueize, type, captures, CEIL_MOD_EXPR))
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
	      case CEIL_MOD_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_333 (res_op, seq, valueize, type, captures, CEIL_MOD_EXPR))
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
				if (gimple_simplify_334 (res_op, seq, valueize, type, captures, CEIL_MOD_EXPR))
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
 && (TREE_CODE_CLASS (CEIL_MOD_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2451;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2451;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2451;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2451:;
					  }
				      }
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
 && (TREE_CODE_CLASS (CEIL_MOD_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2452;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2452;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2452;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2452:;
		      }
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
 && (TREE_CODE_CLASS (CEIL_MOD_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2453;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2453;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2453;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2453:;
		      }
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
gimple_simplify_TRUNC_MOD_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_329 (res_op, seq, valueize, type, captures, TRUNC_MOD_EXPR))
	  return true;
      }
    }
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_330 (res_op, seq, valueize, type, captures, TRUNC_MOD_EXPR))
	  return true;
      }
    }
  if (integer_minus_onep (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_331 (res_op, seq, valueize, type, captures, TRUNC_MOD_EXPR))
	  return true;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_332 (res_op, seq, valueize, type, captures, TRUNC_MOD_EXPR))
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
	      case TRUNC_MOD_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_333 (res_op, seq, valueize, type, captures, TRUNC_MOD_EXPR))
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
				if (gimple_simplify_334 (res_op, seq, valueize, type, captures, TRUNC_MOD_EXPR))
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
				    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
				    if (gimple_simplify_382 (res_op, seq, valueize, type, captures))
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
		  tree _q30_pops[1];
		  if (gimple_power_of_two_cand (_q30, _q30_pops, valueize))
		    {
		      tree _q40 = _q30_pops[0];
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q40 };
			if (gimple_simplify_335 (res_op, seq, valueize, type, captures, TRUNC_MOD_EXPR))
			  return true;
		      }
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
		    if (gimple_simplify_382 (res_op, seq, valueize, type, captures))
		      return true;
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
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (TYPE_SIGN (type) == SIGNED
 && !TREE_OVERFLOW (captures[1])
 && wi::neg_p (wi::to_wide (captures[1]))
 && !TYPE_OVERFLOW_TRAPS (type)
 && !sign_bit_p (captures[1], captures[1])
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2457;
	      {
		res_op->set_op (TRUNC_MOD_EXPR, type, 2);
		res_op->ops[0] = captures[0];
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[1];
		  gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail2457;
		  res_op->ops[1] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1108, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2457:;
	    }
	}
        break;
      }
    default:;
    }
{
  tree _p1_pops[1];
  if (gimple_power_of_two_cand (_p1, _p1_pops, valueize))
    {
      tree _q30 = _p1_pops[0];
      {
	tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
	if (gimple_simplify_335 (res_op, seq, valueize, type, captures, TRUNC_MOD_EXPR))
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
 && (TREE_CODE_CLASS (TRUNC_MOD_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2458;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2458;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2458;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2458:;
					  }
				      }
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
 && (TREE_CODE_CLASS (TRUNC_MOD_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2459;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2459;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2459;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2459:;
		      }
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
 && (TREE_CODE_CLASS (TRUNC_MOD_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2460;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2460;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2460;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2460:;
		      }
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
gimple_simplify_RSHIFT_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
			  if (gimple_simplify_383 (res_op, seq, valueize, type, captures, RSHIFT_EXPR, TRUNC_MOD_EXPR))
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
			  if (gimple_simplify_383 (res_op, seq, valueize, type, captures, RSHIFT_EXPR, FLOOR_MOD_EXPR))
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
				{
 wide_int c = wi::to_wide (captures[2]);
 wide_int n = wi::to_wide (captures[3]);
 bool shift = RSHIFT_EXPR == RSHIFT_EXPR;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2469;
					    {
					      res_op->set_op (PLUS_EXPR, type, 2);
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[1];
						_o1[1] = captures[3];
						gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (lseq, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r1) goto next_after_fail2469;
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
next_after_fail2469:;
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
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2470;
						{
						  res_op->set_op (PLUS_EXPR, type, 2);
						  {
						    tree _o1[2], _r1;
						    _o1[0] = captures[1];
						    _o1[1] = captures[3];
						    gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						    tem_op.resimplify (lseq, valueize);
						    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r1) goto next_after_fail2470;
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
next_after_fail2470:;
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
  if (uniform_integer_cst_p (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_384 (res_op, seq, valueize, type, captures, RSHIFT_EXPR))
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
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
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
				      gimple_seq *lseq = seq;
				      if (lseq
				          && (!single_use (captures[0])))
				        lseq = NULL;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2471;
				      {
					res_op->set_op (NEGATE_EXPR, type, 1);
					{
					  tree _o1[1], _r1;
					  {
					    tree _o2[2], _r2;
					    _o2[0] = captures[1];
					    _o2[1] =  zeros;
					    gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, boolean_type_node, _o2[0], _o2[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail2471;
					    _o1[0] = _r2;
					  }
					  if (type != TREE_TYPE (_o1[0])
					      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					    {
					      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2471;
					    }
					  else
					    _r1 = _o1[0];
					  res_op->ops[0] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1113, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2471:;
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
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[0])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2472;
					  {
					    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[1];
					      _o1[1] =  zeros;
					      gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, bt, _o1[0], _o1[1]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2472;
					      res_op->ops[0] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1114, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail2472:;
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
				      gimple_seq *lseq = seq;
				      if (lseq
				          && (!single_use (captures[0])))
				        lseq = NULL;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2473;
				      {
					res_op->set_op (NEGATE_EXPR, type, 1);
					{
					  tree _o1[1], _r1;
					  {
					    tree _o2[2], _r2;
					    _o2[0] = captures[1];
					    _o2[1] =  zeros;
					    gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, boolean_type_node, _o2[0], _o2[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r2) goto next_after_fail2473;
					    _o1[0] = _r2;
					  }
					  if (type != TREE_TYPE (_o1[0])
					      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
					    {
					      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2473;
					    }
					  else
					    _r1 = _o1[0];
					  res_op->ops[0] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1113, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2473:;
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
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[0])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2474;
					  {
					    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[1];
					      _o1[1] =  zeros;
					      gimple_match_op tem_op (res_op->cond.any_else (), GT_EXPR, bt, _o1[0], _o1[1]);
					      tem_op.resimplify (lseq, valueize);
					      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r1) goto next_after_fail2474;
					      res_op->ops[0] = _r1;
					    }
					    res_op->resimplify (lseq, valueize);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1114, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail2474:;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q21))
		    {
		    case INTEGER_CST:
		      {
			if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q21, 0))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q21 };
			      if (gimple_simplify_394 (res_op, seq, valueize, type, captures))
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
		  case LSHIFT_EXPR:
		    {
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      tree _q31 = gimple_assign_rhs2 (_a1);
		      _q31 = do_valueize (valueize, _q31);
		      switch (TREE_CODE (_q31))
		        {
			case INTEGER_CST:
			  {
			    if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q31, 0))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p1, _q31 };
				  if (gimple_simplify_394 (res_op, seq, valueize, type, captures))
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
if (integer_all_onesp (_p0))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (!TYPE_UNSIGNED (type)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2475;
	  {
	    tree tem;
	    tem = captures[0];
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1115, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail2475:;
	}
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2476;
	{
	  tree tem;
	  tem =  build_zero_cst (type);
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1116, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail2476:;
      }
    }
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_386 (res_op, seq, valueize, type, captures, RSHIFT_EXPR))
	  return true;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_387 (res_op, seq, valueize, type, captures, RSHIFT_EXPR))
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
		    if (gimple_simplify_389 (res_op, seq, valueize, type, captures, RSHIFT_EXPR))
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
	    if (gimple_simplify_390 (res_op, seq, valueize, type, captures, RSHIFT_EXPR))
	      return true;
	  }
	}
      break;
    case VECTOR_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_388 (res_op, seq, valueize, type, captures, RSHIFT_EXPR))
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
			switch (TREE_CODE (_p1))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_391 (res_op, seq, valueize, type, captures, RSHIFT_EXPR))
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
						if (gimple_simplify_392 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, RSHIFT_EXPR))
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
						if (gimple_simplify_392 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR, RSHIFT_EXPR))
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
						if (gimple_simplify_392 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, RSHIFT_EXPR))
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
			  else if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_BSWAP128:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    switch (TREE_CODE (_p1))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
					    if (gimple_simplify_395 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128))
					      return true;
					  }
				          break;
				        }
				      default:;
				      }
			          }
			        break;
			      case CFN_BUILT_IN_BSWAP16:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    switch (TREE_CODE (_p1))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
					    if (gimple_simplify_395 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16))
					      return true;
					  }
				          break;
				        }
				      default:;
				      }
			          }
			        break;
			      case CFN_BUILT_IN_BSWAP32:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    switch (TREE_CODE (_p1))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
					    if (gimple_simplify_395 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32))
					      return true;
					  }
				          break;
				        }
				      default:;
				      }
			          }
			        break;
			      case CFN_BUILT_IN_BSWAP64:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    switch (TREE_CODE (_p1))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
					    if (gimple_simplify_395 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64))
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
				if (gimple_simplify_393 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, RSHIFT_EXPR))
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
				if (gimple_simplify_393 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR, RSHIFT_EXPR))
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
				if (gimple_simplify_393 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, RSHIFT_EXPR))
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
 && (TREE_CODE_CLASS (RSHIFT_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2477;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2477;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2477;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2477:;
					  }
				      }
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
 && (TREE_CODE_CLASS (RSHIFT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2478;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2478;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2478;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2478:;
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
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_395 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128))
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
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_395 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16))
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
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_395 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32))
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
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
			    if (gimple_simplify_395 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64))
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
 && (TREE_CODE_CLASS (RSHIFT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2479;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2479;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2479;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2479:;
		      }
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
					      case PLUS_EXPR:
					        {
						  tree _q40 = gimple_assign_rhs1 (_a3);
						  _q40 = do_valueize (valueize, _q40);
						  tree _q41 = gimple_assign_rhs2 (_a3);
						  _q41 = do_valueize (valueize, _q41);
						  if (tree_swap_operands_p (_q40, _q41))
						    std::swap (_q40, _q41);
						  switch (TREE_CODE (_q40))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q40))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case RSHIFT_EXPR:
							        {
								  tree _q50 = gimple_assign_rhs1 (_a4);
								  _q50 = do_valueize (valueize, _q50);
								  tree _q51 = gimple_assign_rhs2 (_a4);
								  _q51 = do_valueize (valueize, _q51);
								  switch (TREE_CODE (_q51))
								    {
								    case INTEGER_CST:
								      {
									if ((_q41 == _q50 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q50, 0) && types_match (_q41, _q50)))
									  {
									    switch (TREE_CODE (_q41))
									      {
									      case SSA_NAME:
									        if (gimple *_d5 = get_def (valueize, _q41))
									          {
										    if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										      switch (gimple_assign_rhs_code (_a5))
											{
											case PLUS_EXPR:
											  {
											    tree _q90 = gimple_assign_rhs1 (_a5);
											    _q90 = do_valueize (valueize, _q90);
											    tree _q91 = gimple_assign_rhs2 (_a5);
											    _q91 = do_valueize (valueize, _q91);
											    if (tree_swap_operands_p (_q90, _q91))
											      std::swap (_q90, _q91);
											    switch (TREE_CODE (_q90))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q90))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case BIT_AND_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a6);
													    _q100 = do_valueize (valueize, _q100);
													    tree _q101 = gimple_assign_rhs2 (_a6);
													    _q101 = do_valueize (valueize, _q101);
													    if (tree_swap_operands_p (_q100, _q101))
													      std::swap (_q100, _q101);
													    switch (TREE_CODE (_q101))
													      {
													      case INTEGER_CST:
													        {
														  switch (TREE_CODE (_q91))
														    {
														    case SSA_NAME:
														      if (gimple *_d7 = get_def (valueize, _q91))
														        {
															  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
															    switch (gimple_assign_rhs_code (_a7))
															      {
															      case BIT_AND_EXPR:
															        {
																  tree _q130 = gimple_assign_rhs1 (_a7);
																  _q130 = do_valueize (valueize, _q130);
																  tree _q131 = gimple_assign_rhs2 (_a7);
																  _q131 = do_valueize (valueize, _q131);
																  if (tree_swap_operands_p (_q130, _q131))
																    std::swap (_q130, _q131);
																  switch (TREE_CODE (_q130))
																    {
																    case SSA_NAME:
																      if (gimple *_d8 = get_def (valueize, _q130))
																        {
																	  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																	    switch (gimple_assign_rhs_code (_a8))
																	      {
																	      case RSHIFT_EXPR:
																	        {
																		  tree _q140 = gimple_assign_rhs1 (_a8);
																		  _q140 = do_valueize (valueize, _q140);
																		  tree _q141 = gimple_assign_rhs2 (_a8);
																		  _q141 = do_valueize (valueize, _q141);
																		  if ((_q140 == _q100 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q100, 0) && types_match (_q140, _q100)))
																		    {
																		      switch (TREE_CODE (_q140))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d9 = get_def (valueize, _q140))
																			    {
																			      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																			        switch (gimple_assign_rhs_code (_a9))
																				  {
																				  case MINUS_EXPR:
																				    {
																				      tree _q160 = gimple_assign_rhs1 (_a9);
																				      _q160 = do_valueize (valueize, _q160);
																				      tree _q161 = gimple_assign_rhs2 (_a9);
																				      _q161 = do_valueize (valueize, _q161);
																				      switch (TREE_CODE (_q161))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d10 = get_def (valueize, _q161))
																					    {
																					      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																					        switch (gimple_assign_rhs_code (_a10))
																						  {
																						  case BIT_AND_EXPR:
																						    {
																						      tree _q180 = gimple_assign_rhs1 (_a10);
																						      _q180 = do_valueize (valueize, _q180);
																						      tree _q181 = gimple_assign_rhs2 (_a10);
																						      _q181 = do_valueize (valueize, _q181);
																						      if (tree_swap_operands_p (_q180, _q181))
																						        std::swap (_q180, _q181);
																						      switch (TREE_CODE (_q180))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d11 = get_def (valueize, _q180))
																							    {
																							      if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																							        switch (gimple_assign_rhs_code (_a11))
																								  {
																								  case RSHIFT_EXPR:
																								    {
																								      tree _q190 = gimple_assign_rhs1 (_a11);
																								      _q190 = do_valueize (valueize, _q190);
																								      tree _q191 = gimple_assign_rhs2 (_a11);
																								      _q191 = do_valueize (valueize, _q191);
																								      if ((_q190 == _q160 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q160, 0) && types_match (_q190, _q160)))
																									{
																									  switch (TREE_CODE (_q191))
																									    {
																									    case INTEGER_CST:
																									      {
																										switch (TREE_CODE (_q181))
																										  {
																										  case INTEGER_CST:
																										    {
																										      switch (TREE_CODE (_q141))
																										        {
																											case INTEGER_CST:
																											  {
																											    switch (TREE_CODE (_q131))
																											      {
																											      case INTEGER_CST:
																											        {
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
																														      tree captures[12] ATTRIBUTE_UNUSED = { _q50, _q51, _q100, _q101, _q160, _q191, _q181, _q141, _q131, _q31, _q21, _p1 };
																														      if (gimple_simplify_396 (res_op, seq, valueize, type, captures))
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
																											    break;
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
																							  break;
																						        default:;
																						        }
																						      break;
																						    }
																						  default:;
																						  }
																					    }
																					  break;
																				        default:;
																				        }
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
													    switch (TREE_CODE (_q100))
													      {
													      case SSA_NAME:
													        if (gimple *_d7 = get_def (valueize, _q100))
													          {
														    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
														      switch (gimple_assign_rhs_code (_a7))
															{
															case RSHIFT_EXPR:
															  {
															    tree _q110 = gimple_assign_rhs1 (_a7);
															    _q110 = do_valueize (valueize, _q110);
															    tree _q111 = gimple_assign_rhs2 (_a7);
															    _q111 = do_valueize (valueize, _q111);
															    switch (TREE_CODE (_q110))
															      {
															      case SSA_NAME:
															        if (gimple *_d8 = get_def (valueize, _q110))
															          {
																    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																      switch (gimple_assign_rhs_code (_a8))
																	{
																	case MINUS_EXPR:
																	  {
																	    tree _q120 = gimple_assign_rhs1 (_a8);
																	    _q120 = do_valueize (valueize, _q120);
																	    tree _q121 = gimple_assign_rhs2 (_a8);
																	    _q121 = do_valueize (valueize, _q121);
																	    switch (TREE_CODE (_q121))
																	      {
																	      case SSA_NAME:
																	        if (gimple *_d9 = get_def (valueize, _q121))
																	          {
																		    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																		      switch (gimple_assign_rhs_code (_a9))
																			{
																			case BIT_AND_EXPR:
																			  {
																			    tree _q140 = gimple_assign_rhs1 (_a9);
																			    _q140 = do_valueize (valueize, _q140);
																			    tree _q141 = gimple_assign_rhs2 (_a9);
																			    _q141 = do_valueize (valueize, _q141);
																			    if (tree_swap_operands_p (_q140, _q141))
																			      std::swap (_q140, _q141);
																			    switch (TREE_CODE (_q140))
																			      {
																			      case SSA_NAME:
																			        if (gimple *_d10 = get_def (valueize, _q140))
																			          {
																				    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																				      switch (gimple_assign_rhs_code (_a10))
																					{
																					case RSHIFT_EXPR:
																					  {
																					    tree _q150 = gimple_assign_rhs1 (_a10);
																					    _q150 = do_valueize (valueize, _q150);
																					    tree _q151 = gimple_assign_rhs2 (_a10);
																					    _q151 = do_valueize (valueize, _q151);
																					    if ((_q150 == _q120 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q120, 0) && types_match (_q150, _q120)))
																					      {
																						switch (TREE_CODE (_q151))
																						  {
																						  case INTEGER_CST:
																						    {
																						      switch (TREE_CODE (_q141))
																						        {
																							case INTEGER_CST:
																							  {
																							    switch (TREE_CODE (_q111))
																							      {
																							      case INTEGER_CST:
																							        {
																								  switch (TREE_CODE (_q101))
																								    {
																								    case INTEGER_CST:
																								      {
																									switch (TREE_CODE (_q91))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d11 = get_def (valueize, _q91))
																									      {
																										if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																										  switch (gimple_assign_rhs_code (_a11))
																										    {
																										    case BIT_AND_EXPR:
																										      {
																											tree _q210 = gimple_assign_rhs1 (_a11);
																											_q210 = do_valueize (valueize, _q210);
																											tree _q211 = gimple_assign_rhs2 (_a11);
																											_q211 = do_valueize (valueize, _q211);
																											if (tree_swap_operands_p (_q210, _q211))
																											  std::swap (_q210, _q211);
																											if ((_q210 == _q110 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q110, 0) && types_match (_q210, _q110)))
																											  {
																											    switch (TREE_CODE (_q211))
																											      {
																											      case INTEGER_CST:
																											        {
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
																														      tree captures[12] ATTRIBUTE_UNUSED = { _q50, _q51, _q110, _q211, _q120, _q151, _q141, _q111, _q101, _q31, _q21, _p1 };
																														      if (gimple_simplify_396 (res_op, seq, valueize, type, captures))
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
																										      }
																										    default:;
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
																			        break;
																			      default:;
																			      }
																			    break;
																			  }
																			default:;
																			}
																	          }
																	        break;
																	      default:;
																	      }
																	    break;
																	  }
																	default:;
																	}
															          }
															        break;
															      default:;
															      }
															    break;
															  }
															default:;
															}
													          }
													        break;
													      default:;
													      }
													    break;
													  }
													default:;
													}
											          }
											        break;
											      default:;
											      }
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
							          break;
							        }
							      case PLUS_EXPR:
							        {
								  tree _q50 = gimple_assign_rhs1 (_a4);
								  _q50 = do_valueize (valueize, _q50);
								  tree _q51 = gimple_assign_rhs2 (_a4);
								  _q51 = do_valueize (valueize, _q51);
								  if (tree_swap_operands_p (_q50, _q51))
								    std::swap (_q50, _q51);
								  switch (TREE_CODE (_q50))
								    {
								    case SSA_NAME:
								      if (gimple *_d5 = get_def (valueize, _q50))
								        {
									  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									    switch (gimple_assign_rhs_code (_a5))
									      {
									      case BIT_AND_EXPR:
									        {
										  tree _q60 = gimple_assign_rhs1 (_a5);
										  _q60 = do_valueize (valueize, _q60);
										  tree _q61 = gimple_assign_rhs2 (_a5);
										  _q61 = do_valueize (valueize, _q61);
										  if (tree_swap_operands_p (_q60, _q61))
										    std::swap (_q60, _q61);
										  switch (TREE_CODE (_q61))
										    {
										    case INTEGER_CST:
										      {
											switch (TREE_CODE (_q51))
											  {
											  case SSA_NAME:
											    if (gimple *_d6 = get_def (valueize, _q51))
											      {
												if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												  switch (gimple_assign_rhs_code (_a6))
												    {
												    case BIT_AND_EXPR:
												      {
													tree _q90 = gimple_assign_rhs1 (_a6);
													_q90 = do_valueize (valueize, _q90);
													tree _q91 = gimple_assign_rhs2 (_a6);
													_q91 = do_valueize (valueize, _q91);
													if (tree_swap_operands_p (_q90, _q91))
													  std::swap (_q90, _q91);
													switch (TREE_CODE (_q90))
													  {
													  case SSA_NAME:
													    if (gimple *_d7 = get_def (valueize, _q90))
													      {
														if (gassign *_a7 = dyn_cast <gassign *> (_d7))
														  switch (gimple_assign_rhs_code (_a7))
														    {
														    case RSHIFT_EXPR:
														      {
															tree _q100 = gimple_assign_rhs1 (_a7);
															_q100 = do_valueize (valueize, _q100);
															tree _q101 = gimple_assign_rhs2 (_a7);
															_q101 = do_valueize (valueize, _q101);
															if ((_q100 == _q60 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q60, 0) && types_match (_q100, _q60)))
															  {
															    switch (TREE_CODE (_q100))
															      {
															      case SSA_NAME:
															        if (gimple *_d8 = get_def (valueize, _q100))
															          {
																    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																      switch (gimple_assign_rhs_code (_a8))
																	{
																	case MINUS_EXPR:
																	  {
																	    tree _q120 = gimple_assign_rhs1 (_a8);
																	    _q120 = do_valueize (valueize, _q120);
																	    tree _q121 = gimple_assign_rhs2 (_a8);
																	    _q121 = do_valueize (valueize, _q121);
																	    switch (TREE_CODE (_q121))
																	      {
																	      case SSA_NAME:
																	        if (gimple *_d9 = get_def (valueize, _q121))
																	          {
																		    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																		      switch (gimple_assign_rhs_code (_a9))
																			{
																			case BIT_AND_EXPR:
																			  {
																			    tree _q140 = gimple_assign_rhs1 (_a9);
																			    _q140 = do_valueize (valueize, _q140);
																			    tree _q141 = gimple_assign_rhs2 (_a9);
																			    _q141 = do_valueize (valueize, _q141);
																			    if (tree_swap_operands_p (_q140, _q141))
																			      std::swap (_q140, _q141);
																			    switch (TREE_CODE (_q140))
																			      {
																			      case SSA_NAME:
																			        if (gimple *_d10 = get_def (valueize, _q140))
																			          {
																				    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																				      switch (gimple_assign_rhs_code (_a10))
																					{
																					case RSHIFT_EXPR:
																					  {
																					    tree _q150 = gimple_assign_rhs1 (_a10);
																					    _q150 = do_valueize (valueize, _q150);
																					    tree _q151 = gimple_assign_rhs2 (_a10);
																					    _q151 = do_valueize (valueize, _q151);
																					    if ((_q150 == _q120 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q120, 0) && types_match (_q150, _q120)))
																					      {
																						switch (TREE_CODE (_q151))
																						  {
																						  case INTEGER_CST:
																						    {
																						      switch (TREE_CODE (_q141))
																						        {
																							case INTEGER_CST:
																							  {
																							    switch (TREE_CODE (_q101))
																							      {
																							      case INTEGER_CST:
																							        {
																								  switch (TREE_CODE (_q91))
																								    {
																								    case INTEGER_CST:
																								      {
																									switch (TREE_CODE (_q41))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d11 = get_def (valueize, _q41))
																									      {
																										if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																										  switch (gimple_assign_rhs_code (_a11))
																										    {
																										    case RSHIFT_EXPR:
																										      {
																											tree _q210 = gimple_assign_rhs1 (_a11);
																											_q210 = do_valueize (valueize, _q210);
																											tree _q211 = gimple_assign_rhs2 (_a11);
																											_q211 = do_valueize (valueize, _q211);
																											if ((_q210 == _q40 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q40, 0) && types_match (_q210, _q40)))
																											  {
																											    switch (TREE_CODE (_q211))
																											      {
																											      case INTEGER_CST:
																											        {
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
																														      tree captures[12] ATTRIBUTE_UNUSED = { _q40, _q211, _q60, _q61, _q120, _q151, _q141, _q101, _q91, _q31, _q21, _p1 };
																														      if (gimple_simplify_396 (res_op, seq, valueize, type, captures))
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
																										      }
																										    default:;
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
																			        break;
																			      default:;
																			      }
																			    break;
																			  }
																			default:;
																			}
																	          }
																	        break;
																	      default:;
																	      }
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
										  switch (TREE_CODE (_q60))
										    {
										    case SSA_NAME:
										      if (gimple *_d6 = get_def (valueize, _q60))
										        {
											  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
											    switch (gimple_assign_rhs_code (_a6))
											      {
											      case RSHIFT_EXPR:
											        {
												  tree _q70 = gimple_assign_rhs1 (_a6);
												  _q70 = do_valueize (valueize, _q70);
												  tree _q71 = gimple_assign_rhs2 (_a6);
												  _q71 = do_valueize (valueize, _q71);
												  switch (TREE_CODE (_q70))
												    {
												    case SSA_NAME:
												      if (gimple *_d7 = get_def (valueize, _q70))
												        {
													  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
													    switch (gimple_assign_rhs_code (_a7))
													      {
													      case MINUS_EXPR:
													        {
														  tree _q80 = gimple_assign_rhs1 (_a7);
														  _q80 = do_valueize (valueize, _q80);
														  tree _q81 = gimple_assign_rhs2 (_a7);
														  _q81 = do_valueize (valueize, _q81);
														  switch (TREE_CODE (_q81))
														    {
														    case SSA_NAME:
														      if (gimple *_d8 = get_def (valueize, _q81))
														        {
															  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
															    switch (gimple_assign_rhs_code (_a8))
															      {
															      case BIT_AND_EXPR:
															        {
																  tree _q100 = gimple_assign_rhs1 (_a8);
																  _q100 = do_valueize (valueize, _q100);
																  tree _q101 = gimple_assign_rhs2 (_a8);
																  _q101 = do_valueize (valueize, _q101);
																  if (tree_swap_operands_p (_q100, _q101))
																    std::swap (_q100, _q101);
																  switch (TREE_CODE (_q100))
																    {
																    case SSA_NAME:
																      if (gimple *_d9 = get_def (valueize, _q100))
																        {
																	  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																	    switch (gimple_assign_rhs_code (_a9))
																	      {
																	      case RSHIFT_EXPR:
																	        {
																		  tree _q110 = gimple_assign_rhs1 (_a9);
																		  _q110 = do_valueize (valueize, _q110);
																		  tree _q111 = gimple_assign_rhs2 (_a9);
																		  _q111 = do_valueize (valueize, _q111);
																		  if ((_q110 == _q80 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q80, 0) && types_match (_q110, _q80)))
																		    {
																		      switch (TREE_CODE (_q111))
																		        {
																			case INTEGER_CST:
																			  {
																			    switch (TREE_CODE (_q101))
																			      {
																			      case INTEGER_CST:
																			        {
																				  switch (TREE_CODE (_q71))
																				    {
																				    case INTEGER_CST:
																				      {
																					switch (TREE_CODE (_q61))
																					  {
																					  case INTEGER_CST:
																					    {
																					      switch (TREE_CODE (_q51))
																					        {
																						case SSA_NAME:
																						  if (gimple *_d10 = get_def (valueize, _q51))
																						    {
																						      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																						        switch (gimple_assign_rhs_code (_a10))
																							  {
																							  case BIT_AND_EXPR:
																							    {
																							      tree _q170 = gimple_assign_rhs1 (_a10);
																							      _q170 = do_valueize (valueize, _q170);
																							      tree _q171 = gimple_assign_rhs2 (_a10);
																							      _q171 = do_valueize (valueize, _q171);
																							      if (tree_swap_operands_p (_q170, _q171))
																							        std::swap (_q170, _q171);
																							      if ((_q170 == _q70 && ! TREE_SIDE_EFFECTS (_q170)) || (operand_equal_p (_q170, _q70, 0) && types_match (_q170, _q70)))
																								{
																								  switch (TREE_CODE (_q171))
																								    {
																								    case INTEGER_CST:
																								      {
																									switch (TREE_CODE (_q41))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d11 = get_def (valueize, _q41))
																									      {
																										if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																										  switch (gimple_assign_rhs_code (_a11))
																										    {
																										    case RSHIFT_EXPR:
																										      {
																											tree _q200 = gimple_assign_rhs1 (_a11);
																											_q200 = do_valueize (valueize, _q200);
																											tree _q201 = gimple_assign_rhs2 (_a11);
																											_q201 = do_valueize (valueize, _q201);
																											if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																											  {
																											    switch (TREE_CODE (_q201))
																											      {
																											      case INTEGER_CST:
																											        {
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
																														      tree captures[12] ATTRIBUTE_UNUSED = { _q40, _q201, _q70, _q171, _q80, _q111, _q101, _q71, _q61, _q31, _q21, _p1 };
																														      if (gimple_simplify_396 (res_op, seq, valueize, type, captures))
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
																										      }
																										    default:;
																										    }
																									      }
																									    break;
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
											      default:;
											      }
										        }
										      break;
										    default:;
										    }
									          break;
									        }
									      default:;
									      }
								        }
								      break;
								    default:;
								    }
							          break;
							        }
							      default:;
							      }
						        }
						      break;
						    default:;
						    }
					          break;
					        }
					      default:;
					      }
				        }
				      break;
				    default:;
				    }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
		    }
	          break;
	        }
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
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2480;
						    {
						      res_op->set_op (VEC_PERM_EXPR, type, 3);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[2];
							gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2480;
							captures[3] = _r1;
						      }
						      res_op->ops[0] = captures[3];
						      res_op->ops[1] = captures[3];
						      res_op->ops[2] = captures[1];
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1001, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2480:;
						  }
					      }
					    }
				        }
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
gimple_simplify_UNEQ_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
 && (TREE_CODE_CLASS (UNEQ_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2654;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), UNEQ_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2654;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), UNEQ_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2654;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2654:;
					  }
				      }
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
 && (TREE_CODE_CLASS (UNEQ_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2655;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNEQ_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2655;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNEQ_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2655;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2655:;
		      }
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
 && (TREE_CODE_CLASS (UNEQ_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2656;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNEQ_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2656;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNEQ_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2656;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2656:;
		      }
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
	if (gimple_simplify_616 (res_op, seq, valueize, type, captures, UNEQ_EXPR))
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, UNEQ_EXPR, EQ_EXPR))
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, UNEQ_EXPR, UNEQ_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, UNEQ_EXPR, UNEQ_EXPR))
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
	  if (gimple_simplify_614 (res_op, seq, valueize, type, captures, UNEQ_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify_TRUTH_OR_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2687;
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
														if (!_r2) goto next_after_fail2687;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2687;
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
next_after_fail2687:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2688;
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
														  if (!_r3) goto next_after_fail2688;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2688;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2688;
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
next_after_fail2688:;
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
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2689;
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
														if (!_r2) goto next_after_fail2689;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2689;
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
next_after_fail2689:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2690;
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
														  if (!_r3) goto next_after_fail2690;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2690;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2690;
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
next_after_fail2690:;
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
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2691;
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
														if (!_r2) goto next_after_fail2691;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2691;
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
next_after_fail2691:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2692;
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
														  if (!_r3) goto next_after_fail2692;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2692;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2692;
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
next_after_fail2692:;
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
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2693;
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
														if (!_r2) goto next_after_fail2693;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2693;
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
next_after_fail2693:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2694;
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
														  if (!_r3) goto next_after_fail2694;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2694;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2694;
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
next_after_fail2694:;
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
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2695;
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
														if (!_r2) goto next_after_fail2695;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2695;
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
next_after_fail2695:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2696;
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
														  if (!_r3) goto next_after_fail2696;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2696;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2696;
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
next_after_fail2696:;
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
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2697;
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
														if (!_r2) goto next_after_fail2697;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2697;
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
next_after_fail2697:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2698;
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
														  if (!_r3) goto next_after_fail2698;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2698;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2698;
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
next_after_fail2698:;
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
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2699;
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
														if (!_r2) goto next_after_fail2699;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2699;
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
next_after_fail2699:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2700;
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
														  if (!_r3) goto next_after_fail2700;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2700;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2700;
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
next_after_fail2700:;
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
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2701;
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
														if (!_r2) goto next_after_fail2701;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2701;
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
next_after_fail2701:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2702;
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
														  if (!_r3) goto next_after_fail2702;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2702;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2702;
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
next_after_fail2702:;
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
