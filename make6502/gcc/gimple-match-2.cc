/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "gimple-match-auto.h"

bool
gimple_truth_valued_p (tree t, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
    tree captures[1] ATTRIBUTE_UNUSED = { t };
    if (INTEGRAL_TYPE_P (type) && TYPE_PRECISION (type) == 1
)
      {
	{
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	  return true;
	}
      }
  }
  switch (TREE_CODE (t))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, t))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case LT_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case LE_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
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
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
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
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case GE_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case GT_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case UNORDERED_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case ORDERED_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case UNLT_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case UNLE_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case UNGT_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case UNGE_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case UNEQ_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case LTGT_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case TRUTH_AND_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case TRUTH_ANDIF_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case TRUTH_OR_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case TRUTH_ORIF_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case TRUTH_XOR_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      case TRUTH_NOT_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		    {
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 23, __FILE__, __LINE__, false);
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
  return false;
}

bool
gimple_cond_expr_convert_p (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
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
			      case LE_EXPR:
			      case EQ_EXPR:
			      case NE_EXPR:
			      case GE_EXPR:
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
					      CASE_CONVERT:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  switch (TREE_CODE (_p2))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _p2))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      CASE_CONVERT:
							        {
								  tree _q70 = gimple_assign_rhs1 (_a4);
								  _q70 = do_valueize (valueize, _q70);
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
									  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 96, __FILE__, __LINE__, false);
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
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    case LT_EXPR:
		    case LE_EXPR:
		    case EQ_EXPR:
		    case NE_EXPR:
		    case GE_EXPR:
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
					    CASE_CONVERT:
					      {
						tree _q50 = gimple_assign_rhs1 (_a2);
						_q50 = do_valueize (valueize, _q50);
						switch (TREE_CODE (_p2))
						  {
						  case SSA_NAME:
						    if (gimple *_d3 = get_def (valueize, _p2))
						      {
							if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							  switch (gimple_assign_rhs_code (_a3))
							    {
							    CASE_CONVERT:
							      {
								tree _q70 = gimple_assign_rhs1 (_a3);
								_q70 = do_valueize (valueize, _q70);
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
									if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 96, __FILE__, __LINE__, false);
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
			      }
			  }
		        break;
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
gimple_simplify_4 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[3])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail582;
      {
	res_op->set_op (RSHIFT_EXPR, type, 2);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[4];
	  gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail582;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 102, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail582:;
    }
  return false;
}

bool
gimple_simplify_6 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail584;
  {
    res_op->set_op (BIT_NOT_EXPR, type, 1);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail584;
      res_op->ops[0] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 104, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail584:;
  return false;
}

bool
gimple_simplify_8 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])
          || !single_use (captures[1])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail586;
  {
    res_op->set_op (BIT_XOR_EXPR, type, 2);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[2];
      _o1[1] = captures[3];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail586;
      res_op->ops[0] = _r1;
    }
    {
      tree _o1[2], _r1;
      {
	tree _o2[1], _r2;
	_o2[0] = captures[4];
	gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	tem_op.resimplify (NULL, valueize);
	_r2 = maybe_push_res_to_seq (&tem_op, NULL);
	if (!_r2) goto next_after_fail586;
	_o1[0] = _r2;
      }
      _o1[1] = captures[3];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (NULL, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, NULL);
      if (!_r1) goto next_after_fail586;
      res_op->ops[1] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 106, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail586:;
  return false;
}

bool
gimple_simplify_10 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
 && bitwise_inverted_equal_p (captures[0], captures[1], wascmp)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail588;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    res_op->ops[0] =  wascmp
 ? constant_boolean_node (true, type)
 : build_all_ones_cst (TREE_TYPE (captures[0]));
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 108, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail588:;
	}
  }
  return false;
}

bool
gimple_simplify_13 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])
          || !single_use (captures[3])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail591;
  {
    res_op->set_op (BIT_IOR_EXPR, type, 2);
    res_op->ops[0] = captures[1];
    {
      tree _o1[1], _r1;
      _o1[0] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail591;
      res_op->ops[1] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 111, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail591:;
  return false;
}

bool
gimple_simplify_15 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail593;
  {
    res_op->set_op (BIT_XOR_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 113, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail593:;
  return false;
}

bool
gimple_simplify_16 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail594;
  {
    res_op->set_op (BIT_AND_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 114, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail594:;
  return false;
}

bool
gimple_simplify_18 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail596;
  {
    res_op->set_op (BIT_IOR_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 116, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail596:;
  return false;
}

bool
gimple_simplify_19 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail597;
  {
    res_op->set_op (NOP_EXPR, type, 1);
    res_op->ops[0] = captures[0];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 117, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail597:;
  return false;
}

bool
gimple_simplify_21 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (element_precision (type) <= element_precision (TREE_TYPE (captures[0]))
 && element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail599;
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
	      if (!_r1) goto next_after_fail599;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  if (type != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail599;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 119, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail599:;
    }
  return false;
}

bool
gimple_simplify_24 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (opo),
 const enum tree_code ARG_UNUSED (opi))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail602;
  {
    res_op->set_op (BIT_AND_EXPR, type, 2);
    {
      tree _o1[1], _r1;
      _o1[0] = captures[1];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail602;
      res_op->ops[0] = _r1;
    }
    res_op->ops[1] = captures[2];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 122, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail602:;
  return false;
}

bool
gimple_simplify_26 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail604;
      {
	res_op->set_op (BIT_XOR_EXPR, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  if (type != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail604;
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
	      if (!_r1) goto next_after_fail604;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 124, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail604:;
    }
  return false;
}

bool
gimple_simplify_29 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!CONSTANT_CLASS_P (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail607;
      {
	res_op->set_op (bitop, type, 2);
	res_op->ops[0] = captures[0];
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[2];
	  gimple_match_op tem_op (res_op->cond.any_else (), bitop, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (NULL, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, NULL);
	  if (!_r1) goto next_after_fail607;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 126, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail607:;
    }
  else
    {
      {
 tree cst1 = const_binop (bitop, type, captures[0], captures[2]);
	  if (cst1
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail608;
	      {
		res_op->set_op (bitop, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] =  cst1;
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 127, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail608:;
	    }
	  else
	    {
	      {
 tree cst2 = const_binop (bitop, type, captures[1], captures[2]);
		  if (cst2
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail609;
		      {
			res_op->set_op (bitop, type, 2);
			res_op->ops[0] = captures[0];
			res_op->ops[1] =  cst2;
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 128, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail609:;
		    }
	      }
	    }
      }
    }
  return false;
}

bool
gimple_simplify_32 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (invert_tree_comparison (cmp, HONOR_NANS (captures[2])) == icmp
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[5])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail612;
      {
	res_op->set_op (VEC_COND_EXPR, type, 3);
	res_op->ops[0] = captures[1];
	res_op->ops[1] = captures[4];
	res_op->ops[2] = captures[7];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 131, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail612:;
    }
  return false;
}

bool
gimple_simplify_35 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail615;
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
		    if (!_r2) goto next_after_fail615;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] =  wide_int_to_tree (t, c);
	      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, t, _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail615;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 133, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail615:;
      }
    }
  return false;
}

bool
gimple_simplify_38 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[1])
	          || !single_use (captures[4])
	          || !single_use (captures[5])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail619;
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
		    if (!_r2) goto next_after_fail619;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] =  wide_int_to_tree (t, c);
	      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, t, _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail619;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 132, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail619:;
      }
    }
  return false;
}

bool
gimple_simplify_41 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && tree_int_cst_sgn (captures[2]) > 0
 && tree_int_cst_sgn (captures[4]) > 0
 && (tree_nonzero_bits (captures[0]) & tree_nonzero_bits (captures[3])) == 0
)
    {
      {
 tree t = type;
 if (!TYPE_OVERFLOW_WRAPS (t))
 t = unsigned_type_for (t);
 wide_int wone = wi::one (TYPE_PRECISION (t));
 wide_int c = wi::add (wi::lshift (wone, wi::to_wide (captures[2])),
 wi::lshift (wone, wi::to_wide (captures[4])));
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[3])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail622;
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
		    if (!_r2) goto next_after_fail622;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] =  wide_int_to_tree (t,c);
	      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, t, _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail622;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 137, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail622:;
      }
    }
  return false;
}

bool
gimple_simplify_45 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail627;
      {
	res_op->set_op (LT_EXPR, type, 2);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[3];
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail627;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[1]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 142, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail627:;
    }
  return false;
}

bool
gimple_simplify_49 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (fold_real_zero_addition_p (type, captures[0], captures[1], 0)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail633;
      {
	tree tem;
	tem = captures[0];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 148, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail633:;
    }
  return false;
}

bool
gimple_simplify_51 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_UNDEFINED (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail636;
      {
	res_op->set_op (ABS_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 151, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail636:;
    }
  return false;
}

bool
gimple_simplify_53 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && tree_fits_uhwi_p (captures[2])
 && tree_to_uhwi (captures[2]) < element_precision (type)
 && (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 || optab_handler (smul_optab,
 TYPE_MODE (type)) != CODE_FOR_nothing)
)
    {
      {
 tree t = type;
 if (!TYPE_OVERFLOW_WRAPS (t)) t = unsigned_type_for (t);
 wide_int w = wi::set_bit_in_zero (tree_to_uhwi (captures[2]),
 element_precision (type));
 w += 1;
 tree cst = wide_int_to_tree (VECTOR_TYPE_P (t) ? TREE_TYPE (t)
 : t, w);
 cst = build_uniform_cst (t, cst);
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail638;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[0];
		if (t != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (t, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, t, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail638;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] =  cst;
	      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail638;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 153, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail638:;
      }
    }
  return false;
}

bool
gimple_simplify_58 (gimple_match_op *res_op, gimple_seq *seq,
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail644;
	  {
	    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	    res_op->ops[0] = captures[0];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 159, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail644:;
	}
    }
  return false;
}

bool
gimple_simplify_61 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      if (!CONSTANT_CLASS_P (captures[1])
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail653;
		  {
		    res_op->set_op (MINUS_EXPR, type, 2);
		    {
		      tree _o1[2], _r1;
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[0];
			gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail653;
			_o1[0] = _r2;
		      }
		      _o1[1] = captures[2];
		      gimple_match_op tem_op (res_op->cond.any_else (), outer_op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      tem_op.resimplify (NULL, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, NULL);
		      if (!_r1) goto next_after_fail653;
		      res_op->ops[0] = _r1;
		    }
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[1];
		      gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail653;
		      res_op->ops[1] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 168, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail653:;
		}
	    }
	  else
	    {
	      if (!ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1]))
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail654;
		  {
		    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
		    {
		      tree _o1[2], _r1;
		      {
			tree _o2[2], _r2;
			_o2[0] = captures[0];
			{
			  tree _o3[1], _r3;
			  _o3[0] = captures[2];
			  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, TREE_TYPE (_o2[0]), _o3[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r3) goto next_after_fail654;
			  _o2[1] = _r3;
			}
			gimple_match_op tem_op (res_op->cond.any_else (), outer_op, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			tem_op.resimplify (NULL, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, NULL);
			if (!_r2) goto next_after_fail654;
			_o1[0] = _r2;
		      }
		      _o1[1] = captures[1];
		      gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail654;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 169, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail654:;
		}
	      else
		{
		  if (types_match (type, captures[1]) && !TYPE_OVERFLOW_SANITIZED (type)
)
		    {
		      {
 tree cst = const_binop (outer_op, type, captures[0], captures[2]);
			  if (cst && !TREE_OVERFLOW (cst)
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail655;
			      {
				res_op->set_op (MINUS_EXPR, type, 2);
				res_op->ops[0] =  cst;
				res_op->ops[1] = captures[1];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 170, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail655:;
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
gimple_simplify_68 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail662;
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
		      if (!_r1) goto next_after_fail662;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 177, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail662:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_71 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (plusminus))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
 && (!FLOAT_TYPE_P (type) || flag_associative_math)
)
    {
      if (!ALL_FRACT_MODE_P (TYPE_MODE (type))
)
	{
	  if ((!ANY_INTEGRAL_TYPE_P (type)
 || TYPE_OVERFLOW_WRAPS (type)
 || (INTEGRAL_TYPE_P (type)
 && ((tree_expr_nonzero_p (captures[1])
 && (plusminus == MINUS_EXPR
 || expr_not_equal_to (captures[1],
 wi::minus_one (TYPE_PRECISION (type)))))
 || expr_not_equal_to (captures[2],
 (plusminus == PLUS_EXPR
 ? wi::max_value (TYPE_PRECISION (type), SIGNED)
 : wi::min_value (TYPE_PRECISION (type), SIGNED))))))
 && single_use (captures[0])
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail666;
	      {
		res_op->set_op (MULT_EXPR, type, 2);
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] =  build_one_cst (type);
		  gimple_match_op tem_op (res_op->cond.any_else (), plusminus, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail666;
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] = captures[1];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 181, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail666:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_74 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail669;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 184, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail669:;
  return false;
}

bool
gimple_simplify_75 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (VECTOR_TYPE_P (type)
 && known_eq (TYPE_VECTOR_SUBPARTS (type),
 TYPE_VECTOR_SUBPARTS (TREE_TYPE (captures[3])))
 && (TYPE_MODE (TREE_TYPE (type))
 == TYPE_MODE (TREE_TYPE (TREE_TYPE (captures[3]))))
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[1])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail670;
      {
	res_op->set_op (MINUS_EXPR, type, 2);
	res_op->ops[0] = captures[0];
	{
	  tree _o1[1], _r1;
	  {
	    tree _o2[3], _r2;
	    _o2[0] = captures[2];
	    {
	      tree _o3[1], _r3;
	      _o3[0] = captures[3];
	      gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o3[0]), _o3[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r3 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r3) goto next_after_fail670;
	      _o2[1] = _r3;
	    }
	    _o2[2] = captures[4];
	    gimple_match_op tem_op (res_op->cond.any_else (), VEC_COND_EXPR, TREE_TYPE (_o2[1]), _o2[0], _o2[1], _o2[2]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail670;
	    _o1[0] = _r2;
	  }
	  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail670;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 185, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail670:;
    }
  return false;
}

bool
gimple_simplify_81 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_CODE (captures[1]) == SSA_NAME && num_imm_uses (captures[1]) == 2
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail676;
      {
	res_op->set_op (PLUS_EXPR, type, 2);
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[3], _r2;
	    _o2[0] = captures[2];
	    _o2[1] = captures[2];
	    _o2[2] = captures[4];
	    gimple_match_op tem_op (res_op->cond.any_else (), VEC_PERM_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1], _o2[2]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail676;
	    _o1[0] = _r2;
	  }
	  _o1[1] = captures[3];
	  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail676;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] = captures[5];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 191, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail676:;
    }
  return false;
}

bool
gimple_simplify_85 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])
          || !single_use (captures[3])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail680;
  {
    res_op->set_op (MINUS_EXPR, type, 2);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail680;
      res_op->ops[0] = _r1;
    }
    res_op->ops[1] = captures[2];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 195, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail680:;
  return false;
}

bool
gimple_simplify_88 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_OVERFLOW_SANITIZED (type) && !TYPE_OVERFLOW_TRAPS (type)
 && !TYPE_SATURATING (type) && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[0]))
 && !TYPE_OVERFLOW_TRAPS (TREE_TYPE (captures[0]))
 && !TYPE_SATURATING (TREE_TYPE (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail683;
      {
	res_op->set_op (BIT_NOT_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  {
	    tree _o2[2], _r2;
	    _o2[0] = captures[1];
	    _o2[1] = captures[2];
	    gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail683;
	    _o1[0] = _r2;
	  }
	  if (type != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail683;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 198, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail683:;
    }
  return false;
}

bool
gimple_simplify_93 (gimple_match_op *res_op, gimple_seq *seq,
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail688;
	  {
	    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	    res_op->ops[0] = captures[1];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 202, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail688:;
	}
    }
  return false;
}

bool
gimple_simplify_95 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (!ANY_INTEGRAL_TYPE_P (type)
 || TYPE_OVERFLOW_WRAPS (type)
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail691;
	      {
		res_op->set_op (NEGATE_EXPR, type, 1);
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[1];
		  gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, type, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail691;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 205, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail691:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail692;
	      {
		res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[1];
		  gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail692;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 206, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail692:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_100 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 || (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1])))
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail699;
	      {
		res_op->set_op (NOP_EXPR, type, 1);
		res_op->ops[0] = captures[1];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 213, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail699:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_102 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (INTEGRAL_TYPE_P (type)
 && TYPE_OVERFLOW_UNDEFINED (type)
 && element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 && element_precision (type) <= element_precision (TREE_TYPE (captures[2]))
)
	    {
	      {
 tree utype = unsigned_type_for (type);
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail702;
		  {
		    res_op->set_op (NOP_EXPR, type, 1);
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
			    if (!_r2) goto next_after_fail702;
			  }
			else
			  _r2 = _o2[0];
			_o1[0] = _r2;
		      }
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[2];
			if (utype != TREE_TYPE (_o2[0])
			    && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
			  {
			    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail702;
			  }
			else
			  _r2 = _o2[0];
			_o1[1] = _r2;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail702;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 216, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail702:;
	      }
	    }
	  else
	    {
	      if (((element_precision (type) <= element_precision (TREE_TYPE (captures[1])))
 == (element_precision (type) <= element_precision (TREE_TYPE (captures[2]))))
 && (element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 || (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))))
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail703;
		  {
		    res_op->set_op (MINUS_EXPR, type, 2);
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[1];
		      if (type != TREE_TYPE (_o1[0])
		          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
			{
			  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail703;
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
			  if (!_r1) goto next_after_fail703;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[1] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 217, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail703:;
		}
	    }
	}
    }
  return false;
}

bool
gimple_simplify_110 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail711;
  {
    res_op->set_op (BIT_IOR_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 225, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail711:;
  return false;
}

bool
gimple_simplify_112 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (bitwise_inverted_equal_p (captures[0], captures[2], wascmp)
 && (!wascmp || element_precision (type) == 1)
)
	{
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail713;
	  {
	    res_op->set_op (BIT_IOR_EXPR, type, 2);
	    res_op->ops[0] = captures[0];
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[3];
	      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail713;
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 227, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail713:;
	}
  }
  return false;
}

bool
gimple_simplify_118 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail719;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 230, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail719:;
  return false;
}

bool
gimple_simplify_123 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail724;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 232, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail724:;
  return false;
}

bool
gimple_simplify_128 (gimple_match_op *res_op, gimple_seq *seq,
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
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail729;
  {
    res_op->set_op (BIT_NOT_EXPR, type, 1);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail729;
      res_op->ops[0] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 238, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail729:;
  return false;
}

bool
gimple_simplify_134 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail735;
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
		  if (!_r1) goto next_after_fail735;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 242, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail735:;
	}
      else
	{
	  if (single_use (captures[0]) && single_use (captures[1])
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail736;
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
		      if (!_r1) goto next_after_fail736;
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
next_after_fail736:;
	    }
	}
    }
  return false;
}

bool
gimple_simplify_140 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail745;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 246, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail745:;
  return false;
}

bool
gimple_simplify_145 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail750;
  {
    res_op->set_op (bitop, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[2];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 247, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail750:;
  return false;
}

bool
gimple_simplify_148 (gimple_match_op *res_op, gimple_seq *seq,
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail754;
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
		  if (!_r3) goto next_after_fail754;
	        }
	      else
	        _r3 = _o3[0];
	      _o2[1] = _r3;
	    }
	    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail754;
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
		if (!_r2) goto next_after_fail754;
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail754;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 248, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail754:;
    }
  return false;
}

bool
gimple_simplify_155 (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail764;
	      {
		tree tem;
		tem = captures[3];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 260, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail764:;
	    }
	  else
	    {
	      if (code1 == NE_EXPR && val && allbits
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail765;
		  {
		    tree tem;
		    tem =  constant_boolean_node (true, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 261, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail765:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail766;
		      {
			tree tem;
			tem = captures[0];
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 262, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail766:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail767;
			  {
			    res_op->set_op (GE_EXPR, type, 2);
			    res_op->ops[0] = captures[4];
			    res_op->ops[1] = captures[5];
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 263, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail767:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail768;
			      {
				res_op->set_op (LE_EXPR, type, 2);
				res_op->ops[0] = captures[4];
				res_op->ops[1] = captures[5];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 264, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail768:;
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail769;
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
					  if (!_r1) goto next_after_fail769;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 265, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail769:;
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
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail770;
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
					      if (!_r1) goto next_after_fail770;
					    }
					  else
					    _r1 = _o1[0];
					  res_op->ops[1] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 266, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail770:;
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
gimple_simplify_168 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail809;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 279, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail809:;
  return false;
}

bool
gimple_simplify_170 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail811;
  {
    tree tem;
    tem = captures[1];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 280, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail811:;
  return false;
}

bool
gimple_simplify_172 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (logic),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (ext))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TREE_CODE (captures[1]) != INTEGER_CST
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])
              || !single_use (captures[3])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail813;
      {
	res_op->set_op (op, type, 2);
	res_op->ops[0] = captures[1];
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[2];
	  _o1[1] = captures[4];
	  gimple_match_op tem_op (res_op->cond.any_else (), ext, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail813;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 281, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail813:;
    }
  return false;
}

bool
gimple_simplify_176 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail817;
      {
	res_op->set_op (NEGATE_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail817;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 282, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail817:;
    }
  return false;
}

bool
gimple_simplify_179 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail820;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail820;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 282, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail820:;
    }
  return false;
}

bool
gimple_simplify_183 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail824;
      {
	res_op->set_op (NEGATE_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail824;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 282, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail824:;
    }
  return false;
}

bool
gimple_simplify_188 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail829;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail829;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 283, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail829:;
    }
  return false;
}

bool
gimple_simplify_195 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (! FLOAT_TYPE_P (type)
 && tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail836;
      {
	res_op->set_op (LSHIFT_EXPR, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 288, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail836:;
    }
  return false;
}

bool
gimple_simplify_199 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail841;
	  {
	    res_op->set_op (ABSU_EXPR, type, 1);
	    res_op->ops[0] = captures[0];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 290, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail841:;
	}
      else
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail842;
	  {
	    res_op->set_op (ABS_EXPR, type, 1);
	    res_op->ops[0] = captures[0];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 291, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail842:;
	}
    }
  return false;
}

bool
gimple_simplify_205 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_CODE (captures[3]) != INTEGER_CST
 && single_use (captures[0])
 && !integer_zerop (captures[2]) && !integer_minus_onep (captures[2])
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail849;
      {
	res_op->set_op (MULT_EXPR, type, 2);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[3];
	  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail849;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 297, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail849:;
    }
  return false;
}

bool
gimple_simplify_210 (gimple_match_op *res_op, gimple_seq *seq,
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail854;
      {
	res_op->set_op (POW, type, 2);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[4];
	  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail854;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 302, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail854:;
    }
  return false;
}

bool
gimple_simplify_214 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_PRECISION (TREE_TYPE (captures[1])) == 1
)
    {
      gimple_seq *lseq = seq;
      if (lseq
          && (!single_use (captures[0])))
        lseq = NULL;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail858;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[2];
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail858;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 306, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail858:;
    }
  return false;
}

bool
gimple_simplify_220 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && integer_nonzerop (captures[2])
 && !TREE_OVERFLOW (captures[2])
 && !TREE_OVERFLOW (captures[3])
)
    {
      {
 tree lo, hi; bool neg_overflow;
 enum tree_code code = fold_div_compare (cmp, captures[2], captures[3], &lo, &hi,
 &neg_overflow);
	  if (code == LT_EXPR || code == GE_EXPR
)
	    {
	      if (TREE_OVERFLOW (lo)
)
		{
		  gimple_seq *lseq = seq;
		  if (lseq
		      && (!single_use (captures[0])))
		    lseq = NULL;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail867;
		  {
		    tree tem;
		    tem =  build_int_cst (type, (code == LT_EXPR) ^ neg_overflow);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 314, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail867:;
		}
	      else
		{
		  if (code == LT_EXPR
)
		    {
		      gimple_seq *lseq = seq;
		      if (lseq
		          && (!single_use (captures[0])))
		        lseq = NULL;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail868;
		      {
			res_op->set_op (LT_EXPR, type, 2);
			res_op->ops[0] = captures[1];
			res_op->ops[1] =  lo;
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 315, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail868:;
		    }
		  else
		    {
		      gimple_seq *lseq = seq;
		      if (lseq
		          && (!single_use (captures[0])))
		        lseq = NULL;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail869;
		      {
			res_op->set_op (GE_EXPR, type, 2);
			res_op->ops[0] = captures[1];
			res_op->ops[1] =  lo;
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 316, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail869:;
		    }
		}
	    }
	  else
	    {
	      if (code == LE_EXPR || code == GT_EXPR
)
		{
		  if (TREE_OVERFLOW (hi)
)
		    {
		      gimple_seq *lseq = seq;
		      if (lseq
		          && (!single_use (captures[0])))
		        lseq = NULL;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail870;
		      {
			tree tem;
			tem =  build_int_cst (type, (code == LE_EXPR) ^ neg_overflow);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 317, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail870:;
		    }
		  else
		    {
		      if (code == LE_EXPR
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[0])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail871;
			  {
			    res_op->set_op (LE_EXPR, type, 2);
			    res_op->ops[0] = captures[1];
			    res_op->ops[1] =  hi;
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 318, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail871:;
			}
		      else
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[0])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail872;
			  {
			    res_op->set_op (GT_EXPR, type, 2);
			    res_op->ops[0] = captures[1];
			    res_op->ops[1] =  hi;
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 319, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail872:;
			}
		    }
		}
	      else
		{
		  if (!lo && !hi
)
		    {
		      gimple_seq *lseq = seq;
		      if (lseq
		          && (!single_use (captures[0])))
		        lseq = NULL;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail873;
		      {
			tree tem;
			tem =  build_int_cst (type, code == NE_EXPR);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 320, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail873:;
		    }
		  else
		    {
		      if (code == EQ_EXPR && !hi
)
			{
			  gimple_seq *lseq = seq;
			  if (lseq
			      && (!single_use (captures[0])))
			    lseq = NULL;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail874;
			  {
			    res_op->set_op (GE_EXPR, type, 2);
			    res_op->ops[0] = captures[1];
			    res_op->ops[1] =  lo;
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 321, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail874:;
			}
		      else
			{
			  if (code == EQ_EXPR && !lo
)
			    {
			      gimple_seq *lseq = seq;
			      if (lseq
			          && (!single_use (captures[0])))
			        lseq = NULL;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail875;
			      {
				res_op->set_op (LE_EXPR, type, 2);
				res_op->ops[0] = captures[1];
				res_op->ops[1] =  hi;
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 322, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail875:;
			    }
			  else
			    {
			      if (code == NE_EXPR && !hi
)
				{
				  gimple_seq *lseq = seq;
				  if (lseq
				      && (!single_use (captures[0])))
				    lseq = NULL;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail876;
				  {
				    res_op->set_op (LT_EXPR, type, 2);
				    res_op->ops[0] = captures[1];
				    res_op->ops[1] =  lo;
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 323, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail876:;
				}
			      else
				{
				  if (code == NE_EXPR && !lo
)
				    {
				      gimple_seq *lseq = seq;
				      if (lseq
				          && (!single_use (captures[0])))
				        lseq = NULL;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail877;
				      {
					res_op->set_op (GT_EXPR, type, 2);
					res_op->ops[0] = captures[1];
					res_op->ops[1] =  hi;
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 324, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail877:;
				    }
				  else
				    {
				      if (0
)
					{
					  gimple_seq *lseq = seq;
					  if (lseq
					      && (!single_use (captures[0])))
					    lseq = NULL;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail878;
					  {
					    tree tem;
					    tem =  build_range_check (UNKNOWN_LOCATION, type, captures[1], code == EQ_EXPR,
 lo, hi);
					    res_op->set_value (tem);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 325, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail878:;
					}
				      else
					{
					  {
 tree etype = range_check_type (TREE_TYPE (captures[1]));
 if (etype)
 {
 hi = fold_convert (etype, hi);
 lo = fold_convert (etype, lo);
 hi = const_binop (MINUS_EXPR, etype, hi, lo);
 }
					      if (etype && hi && !TREE_OVERFLOW (hi)
)
						{
						  if (code == EQ_EXPR
)
						    {
						      gimple_seq *lseq = seq;
						      if (lseq
						          && (!single_use (captures[0])))
						        lseq = NULL;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail879;
						      {
							res_op->set_op (LE_EXPR, type, 2);
							{
							  tree _o1[2], _r1;
							  {
							    tree _o2[1], _r2;
							    _o2[0] = captures[1];
							    if (etype != TREE_TYPE (_o2[0])
							        && !useless_type_conversion_p (etype, TREE_TYPE (_o2[0])))
							      {
								gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, etype, _o2[0]);
								tem_op.resimplify (lseq, valueize);
								_r2 = maybe_push_res_to_seq (&tem_op, lseq);
								if (!_r2) goto next_after_fail879;
							      }
							    else
							      _r2 = _o2[0];
							    _o1[0] = _r2;
							  }
							  _o1[1] =  lo;
							  gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							  tem_op.resimplify (lseq, valueize);
							  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
							  if (!_r1) goto next_after_fail879;
							  res_op->ops[0] = _r1;
							}
							res_op->ops[1] =  hi;
							res_op->resimplify (lseq, valueize);
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 326, __FILE__, __LINE__, true);
							return true;
						      }
next_after_fail879:;
						    }
						  else
						    {
						      gimple_seq *lseq = seq;
						      if (lseq
						          && (!single_use (captures[0])))
						        lseq = NULL;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail880;
						      {
							res_op->set_op (GT_EXPR, type, 2);
							{
							  tree _o1[2], _r1;
							  {
							    tree _o2[1], _r2;
							    _o2[0] = captures[1];
							    if (etype != TREE_TYPE (_o2[0])
							        && !useless_type_conversion_p (etype, TREE_TYPE (_o2[0])))
							      {
								gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, etype, _o2[0]);
								tem_op.resimplify (lseq, valueize);
								_r2 = maybe_push_res_to_seq (&tem_op, lseq);
								if (!_r2) goto next_after_fail880;
							      }
							    else
							      _r2 = _o2[0];
							    _o1[0] = _r2;
							  }
							  _o1[1] =  lo;
							  gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							  tem_op.resimplify (lseq, valueize);
							  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
							  if (!_r1) goto next_after_fail880;
							  res_op->ops[0] = _r1;
							}
							res_op->ops[1] =  hi;
							res_op->resimplify (lseq, valueize);
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 327, __FILE__, __LINE__, true);
							return true;
						      }
next_after_fail880:;
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
gimple_simplify_252 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (single_use (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail925;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[1];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 370, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail925:;
    }
  return false;
}

bool
gimple_simplify_256 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail934;
  {
    tree tem;
    tem =  constant_boolean_node (cmp == NE_EXPR, type);
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 379, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail934:;
  return false;
}

bool
gimple_simplify_258 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail936;
	  {
	    res_op->set_op (scmp, type, 2);
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[1];
	      if (otype != TREE_TYPE (_o1[0])
	          && !useless_type_conversion_p (otype, TREE_TYPE (_o1[0])))
		{
		  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, otype, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail936;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[0] = _r1;
	    }
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail936;
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 381, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail936:;
      }
    }
  return false;
}

bool
gimple_simplify_264 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (sq),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && ! flag_errno_math
)
    {
      if (! HONOR_NANS (captures[0])
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail974;
	  {
	    res_op->set_op (cmp, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] = captures[1];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 418, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail974:;
	}
    }
  return false;
}

bool
gimple_simplify_268 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (scmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 || (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (cmp == EQ_EXPR
 || cmp == NE_EXPR
 || TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail987;
      {
	res_op->set_op (scmp, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 431, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail987:;
    }
  return false;
}

bool
gimple_simplify_269 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (scmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 || (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (cmp == EQ_EXPR
 || cmp == NE_EXPR
 || TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))))
)
    {
      {
 tree tem = const_unop (NEGATE_EXPR, TREE_TYPE (captures[0]), captures[1]);
	  if (tem && !TREE_OVERFLOW (tem)
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail988;
	      {
		res_op->set_op (scmp, type, 2);
		res_op->ops[0] = captures[0];
		res_op->ops[1] =  tem;
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 432, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail988:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_271 (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail996;
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
next_after_fail996:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail997;
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
		      if (!_r1) goto next_after_fail997;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[1] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 434, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail997:;
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail998;
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
			  if (!_r1) goto next_after_fail998;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[1] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 435, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail998:;
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail999;
				  {
				    tree tem;
				    tem =  constant_boolean_node (cmp == EQ_EXPR ? false : true, type);
				    res_op->set_value (tem);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 436, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail999:;
				}
			      else
				{
				  if (cmp == LT_EXPR || cmp == LE_EXPR
)
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1000;
				      {
					tree tem;
					tem =  constant_boolean_node (above ? true : false, type);
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 437, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1000:;
				    }
				  else
				    {
				      if (cmp == GT_EXPR || cmp == GE_EXPR
)
					{
					  gimple_seq *lseq = seq;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1001;
					  {
					    tree tem;
					    tem =  constant_boolean_node (above ? false : true, type);
					    res_op->set_value (tem);
					    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 438, __FILE__, __LINE__, true);
					    return true;
					  }
next_after_fail1001:;
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1002;
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
			  if (!_r1) goto next_after_fail1002;
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
			  if (!_r1) goto next_after_fail1002;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[1] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 439, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1002:;
		}
	  }
	}
    }
  return false;
}

bool
gimple_simplify_294 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_UNSIGNED (TREE_TYPE (captures[2]))
 && (TYPE_PRECISION (TREE_TYPE (captures[1]))
 >= 2 * TYPE_PRECISION (TREE_TYPE (captures[2])))
 && tree_fits_uhwi_p (captures[4])
 && tree_to_uhwi (captures[4]) == TYPE_PRECISION (TREE_TYPE (captures[2]))
 && types_match (captures[2], captures[3])
 && type_has_mode_precision_p (TREE_TYPE (captures[2]))
 && (optab_handler (umulv4_optab, TYPE_MODE (TREE_TYPE (captures[2])))
 != CODE_FOR_nothing)
)
    {
      {
 tree t = TREE_TYPE (captures[2]), cpx = build_complex_type (t);
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1046;
	  {
	    res_op->set_op (cmp, type, 2);
	    {
	      tree _o1[1], _r1;
	      {
		tree _o2[2], _r2;
		_o2[0] = captures[2];
		_o2[1] = captures[3];
		gimple_match_op tem_op (res_op->cond.any_else (), CFN_MUL_OVERFLOW, cpx, _o2[0], _o2[1]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1046;
		_o1[0] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o1[0])), _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1046;
	      res_op->ops[0] = _r1;
	    }
	    res_op->ops[1] =  build_zero_cst (t);
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 473, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1046:;
      }
    }
  return false;
}

bool
gimple_simplify_299 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree type0 = TREE_TYPE (captures[1]);
 int prec = TYPE_PRECISION (type0);
      if (prec <= MAX_FIXED_MODE_SIZE
)
	{
	  if (tree_int_cst_sgn (captures[2]) < 0 || wi::to_widest (captures[2]) >= prec
)
	    {
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1062;
	      {
		tree tem;
		tem =  constant_boolean_node (op == EQ_EXPR ? false : true, type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 489, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1062:;
	    }
	  else
	    {
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1063;
	      {
		res_op->set_op (op, type, 2);
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[1];
		  _o1[1] =  wide_int_to_tree (type0,
 wi::mask (tree_to_uhwi (captures[2]) + 1,
 false, prec));
		  gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1063;
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] =  wide_int_to_tree (type0,
 wi::shifted_mask (tree_to_uhwi (captures[2]), 1,
 false, prec));
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 490, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1063:;
	    }
	}
  }
  return false;
}

bool
gimple_simplify_304 (gimple_match_op *res_op, gimple_seq *seq,
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1071;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[1];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 498, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1071:;
    }
  return false;
}

bool
gimple_simplify_309 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1078;
  {
    res_op->set_op (copysigns, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[2];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 505, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1078:;
  return false;
}

bool
gimple_simplify_312 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1081;
  {
    res_op->set_op (ABS_EXPR, type, 1);
    res_op->ops[0] = captures[0];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 508, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1081:;
  return false;
}

bool
gimple_simplify_318 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!ALL_FRACT_MODE_P (TYPE_MODE (type))
 && !integer_zerop (captures[0])
 && (!flag_non_call_exceptions || tree_expr_nonzero_p (captures[0]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1089;
      {
	tree tem;
	tem =  build_one_cst (type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 514, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1089:;
    }
  return false;
}

bool
gimple_simplify_322 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_pow2p (captures[2])
 && tree_int_cst_sgn (captures[2]) > 0
 && tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
 && wi::to_wide (captures[2]) + wi::to_wide (captures[1]) == 0
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1094;
      {
	res_op->set_op (RSHIFT_EXPR, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  if (type != TREE_TYPE (_o1[0])
	      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
	    {
	      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1094;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] =  build_int_cst (integer_type_node,
 wi::exact_log2 (wi::to_wide (captures[2])));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 519, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1094:;
    }
  return false;
}

bool
gimple_simplify_328 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (floor_divmod),
 const enum tree_code ARG_UNUSED (trunc_divmod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((INTEGRAL_TYPE_P (type) || VECTOR_INTEGER_TYPE_P (type))
 && TYPE_UNSIGNED (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1103;
      {
	res_op->set_op (trunc_divmod, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 528, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1103:;
    }
  return false;
}

bool
gimple_simplify_330 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1105;
  {
    tree tem;
    tem =  build_zero_cst (type);
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 530, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1105:;
  return false;
}

bool
gimple_simplify_336 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (FLOAT_TYPE_P (type)
 && ! HONOR_NANS (type)
 && ! HONOR_INFINITIES (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1111;
      {
	tree tem;
	tem =  build_minus_one_cst (type);
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 536, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1111:;
    }
  return false;
}

bool
gimple_simplify_342 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (TANH),
 const combined_fn ARG_UNUSED (SINH),
 const combined_fn ARG_UNUSED (COSH))
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1119;
      {
	res_op->set_op (RDIV_EXPR, type, 2);
	res_op->ops[0] = build_one_cst (type);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  gimple_match_op tem_op (res_op->cond.any_else (), COSH, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1119;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 544, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1119:;
    }
  return false;
}

bool
gimple_simplify_347 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1124;
	      {
		res_op->set_op (op, type, 2);
		res_op->ops[0] = captures[1];
		res_op->ops[1] = captures[2];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 549, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1124:;
	    }
	  else
	    {
	      if (real_less (TREE_REAL_CST_PTR (captures[0]), &dconst0)
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1125;
		  {
		    res_op->set_op (neg_op, type, 2);
		    res_op->ops[0] = captures[1];
		    res_op->ops[1] = captures[2];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 550, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1125:;
		}
	    }
	}
    }
  return false;
}

bool
gimple_simplify_356 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (comb))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1136;
  {
    res_op->set_op (comb, type, 2);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[0];
      _o1[1] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), cmp, type, _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1136;
      res_op->ops[0] = _r1;
    }
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), cmp, type, _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1136;
      res_op->ops[1] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 561, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1136:;
  return false;
}

bool
gimple_simplify_365 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (single_use (captures[0])
 && ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_UNSIGNED (TREE_TYPE (captures[1]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1177;
      {
	res_op->set_op (cmp, type, 2);
	res_op->ops[0] = captures[2];
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 585, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1177:;
    }
  return false;
}

bool
gimple_simplify_368 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (out))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1180;
      {
	res_op->set_op (out, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o1[0])), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1180;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[1]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 588, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1180:;
    }
  return false;
}

bool
gimple_simplify_372 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool ok = true;
 HOST_WIDE_INT val = 0;
 if (!tree_fits_shwi_p (captures[2]))
 ok = false;
 else
 {
 val = tree_to_shwi (captures[2]);
 if (op == GT_EXPR || op == LE_EXPR)
 {
 if (val == HOST_WIDE_INT_MAX)
 ok = false;
 else
 val++;
 }
 }
 tree type0 = TREE_TYPE (captures[1]);
 int prec = TYPE_PRECISION (type0);
      if (ok && prec <= MAX_FIXED_MODE_SIZE
)
	{
	  if (val <= 0
)
	    {
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1187;
	      {
		tree tem;
		tem =  constant_boolean_node (cmp == EQ_EXPR ? true : false, type);
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 595, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1187:;
	    }
	  else
	    {
	      if (val >= prec
)
		{
		  gimple_seq *lseq = seq;
		  if (lseq
		      && (!single_use (captures[0])))
		    lseq = NULL;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1188;
		  {
		    tree tem;
		    tem =  constant_boolean_node (cmp == EQ_EXPR ? false : true, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 596, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1188:;
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (lseq
		      && (!single_use (captures[0])))
		    lseq = NULL;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1189;
		  {
		    res_op->set_op (cmp, type, 2);
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      _o1[1] =  wide_int_to_tree (type0,
 wi::mask (val, false, prec));
		      gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1189;
		      res_op->ops[0] = _r1;
		    }
		    res_op->ops[1] =  build_zero_cst (type0);
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 597, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1189:;
		}
	    }
	}
  }
  return false;
}

bool
gimple_simplify_378 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (out))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1197;
      {
	res_op->set_op (out, type, 2);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o1[0])), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1197;
	  res_op->ops[0] = _r1;
	}
	res_op->ops[1] =  build_zero_cst (TREE_TYPE (captures[1]));
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 587, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1197:;
    }
  return false;
}

bool
gimple_simplify_383 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (shift),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_pow2p (captures[3]) && tree_int_cst_sgn (captures[3]) > 0
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1206;
      {
	res_op->set_op (shift, type, 2);
	res_op->ops[0] = captures[0];
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  {
	    tree _o2[2], _r2;
	    _o2[0] = captures[2];
	    _o2[1] =  build_int_cst (TREE_TYPE (captures[2]),
 1);
	    gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	    tem_op.resimplify (lseq, valueize);
	    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
	    if (!_r2) goto next_after_fail1206;
	    _o1[1] = _r2;
	  }
	  gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1206;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 610, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1206:;
    }
  return false;
}

bool
gimple_simplify_388 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (shiftrotate))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree tem = uniform_vector_p (captures[1]);
      if (tem
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1211;
	  {
	    res_op->set_op (shiftrotate, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] =  tem;
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 615, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1211:;
	}
  }
  return false;
}

bool
gimple_simplify_392 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	      gimple_seq *lseq = seq;
	      if (lseq
	          && (!single_use (captures[0])
	              || !single_use (captures[1])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1218;
	      {
		res_op->set_op (bit_op, type, 2);
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[2];
		    if (type != TREE_TYPE (_o2[0])
		        && !useless_type_conversion_p (type, TREE_TYPE (_o2[0])))
		      {
			gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1218;
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  _o1[1] = captures[4];
		  gimple_match_op tem_op (res_op->cond.any_else (), shift, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1218;
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] =  mask;
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 621, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1218:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_396 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 unsigned prec = TYPE_PRECISION (type);
 int shift = (64 - prec) & 63;
 unsigned HOST_WIDE_INT c1
 = HOST_WIDE_INT_UC (0x0101010101010101) >> shift;
 unsigned HOST_WIDE_INT c2
 = HOST_WIDE_INT_UC (0x0F0F0F0F0F0F0F0F) >> shift;
 unsigned HOST_WIDE_INT c3
 = HOST_WIDE_INT_UC (0x3333333333333333) >> shift;
 unsigned HOST_WIDE_INT c4
 = HOST_WIDE_INT_UC (0x5555555555555555) >> shift;
      if (prec >= 16
 && prec <= 64
 && pow2p_hwi (prec)
 && TYPE_UNSIGNED (type)
 && integer_onep (captures[5])
 && wi::to_widest (captures[7]) == 2
 && wi::to_widest (captures[1]) == 4
 && wi::to_widest (captures[11]) == prec - 8
 && tree_to_uhwi (captures[10]) == c1
 && tree_to_uhwi (captures[9]) == c2
 && tree_to_uhwi (captures[8]) == c3
 && tree_to_uhwi (captures[3]) == c3
 && tree_to_uhwi (captures[6]) == c4
)
	{
	  if (direct_internal_fn_supported_p (IFN_POPCOUNT, type,
 OPTIMIZE_FOR_BOTH)
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1226;
	      {
		res_op->set_op (NOP_EXPR, type, 1);
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[4];
		  gimple_match_op tem_op (res_op->cond.any_else (), CFN_POPCOUNT, type, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1226;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 628, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1226:;
	    }
	  else
	    {
	      {
 tree half_type = NULL_TREE;
 opt_machine_mode m = mode_for_size ((prec + 1) / 2, MODE_INT, 1);
 int half_prec = 8;
 if (m.exists ()
 && m.require () != TYPE_MODE (type))
 {
 half_prec = GET_MODE_PRECISION (as_a <scalar_int_mode> (m));
 half_type = build_nonstandard_integer_type (half_prec, 1);
 }
 gcc_assert (half_prec > 2);
		  if (half_type != NULL_TREE
 && direct_internal_fn_supported_p (IFN_POPCOUNT, half_type,
 OPTIMIZE_FOR_BOTH)
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1227;
		      {
			res_op->set_op (NOP_EXPR, type, 1);
			{
			  tree _o1[2], _r1;
			  {
			    tree _o2[1], _r2;
			    {
			      tree _o3[1], _r3;
			      _o3[0] = captures[4];
			      if (half_type != TREE_TYPE (_o3[0])
			          && !useless_type_conversion_p (half_type, TREE_TYPE (_o3[0])))
				{
				  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, half_type, _o3[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r3) goto next_after_fail1227;
			        }
			      else
			        _r3 = _o3[0];
			      _o2[0] = _r3;
			    }
			    gimple_match_op tem_op (res_op->cond.any_else (), CFN_POPCOUNT, half_type, _o2[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail1227;
			    _o1[0] = _r2;
			  }
			  {
			    tree _o2[1], _r2;
			    {
			      tree _o3[1], _r3;
			      {
				tree _o4[2], _r4;
				_o4[0] = captures[4];
				_o4[1] =  build_int_cst (integer_type_node, half_prec);
				gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o4[0]), _o4[0], _o4[1]);
				tem_op.resimplify (lseq, valueize);
				_r4 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r4) goto next_after_fail1227;
				_o3[0] = _r4;
			      }
			      if (half_type != TREE_TYPE (_o3[0])
			          && !useless_type_conversion_p (half_type, TREE_TYPE (_o3[0])))
				{
				  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, half_type, _o3[0]);
				  tem_op.resimplify (lseq, valueize);
				  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r3) goto next_after_fail1227;
			        }
			      else
			        _r3 = _o3[0];
			      _o2[0] = _r3;
			    }
			    gimple_match_op tem_op (res_op->cond.any_else (), CFN_POPCOUNT, half_type, _o2[0]);
			    tem_op.resimplify (lseq, valueize);
			    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			    if (!_r2) goto next_after_fail1227;
			    _o1[1] = _r2;
			  }
			  gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail1227;
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 629, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1227:;
		    }
	      }
	    }
	}
  }
  return false;
}

bool
gimple_simplify_411 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1255;
	  {
	    res_op->set_op (RDIV_EXPR, type, 2);
	    res_op->ops[0] =  t_one;
	    {
	      tree _o1[1], _r1;
	      {
		tree _o2[2], _r2;
		{
		  tree _o3[2], _r3;
		  _o3[0] =  t_one;
		  _o3[1] = captures[1];
		  gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r3) goto next_after_fail1255;
		  _o2[0] = _r3;
		}
		{
		  tree _o3[2], _r3;
		  _o3[0] =  t_one;
		  _o3[1] = captures[1];
		  gimple_match_op tem_op (res_op->cond.any_else (), PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r3) goto next_after_fail1255;
		  _o2[1] = _r3;
		}
		gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1255;
		_o1[0] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), sqrts, TREE_TYPE (_o1[0]), _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1255;
	      res_op->ops[1] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 645, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1255:;
      }
    }
  return false;
}

bool
gimple_simplify_414 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POW))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1259;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 649, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1259:;
  return false;
}

bool
gimple_simplify_416 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns),
 const combined_fn ARG_UNUSED (hypots))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1261;
  {
    res_op->set_op (hypots, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[2];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 651, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1261:;
  return false;
}

bool
gimple_simplify_418 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (HYPOT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1263;
  {
    res_op->set_op (ABS_EXPR, type, 1);
    res_op->ops[0] = captures[0];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 653, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1263:;
  return false;
}

bool
gimple_simplify_421 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (! TYPE_UNSIGNED (type)
 && ! HONOR_SIGN_DEPENDENT_ROUNDING (type)
 && single_use (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1266;
      {
	res_op->set_op (MULT_EXPR, type, 2);
	res_op->ops[0] = captures[1];
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[2];
	  gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1266;
	  res_op->ops[1] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 656, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1266:;
    }
  return false;
}

bool
gimple_simplify_425 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fmas))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (single_use (captures[0])
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1271;
	  {
	    res_op->set_op (CFN_FNMS, type, 3);
	    res_op->ops[0] = captures[1];
	    res_op->ops[1] = captures[2];
	    res_op->ops[2] = captures[3];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 661, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1271:;
	}
    }
  return false;
}

bool
gimple_simplify_432 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[3])
          || !single_use (captures[4])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1278;
  {
    res_op->set_op (BIT_AND_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    {
      tree _o1[1], _r1;
      _o1[0] = captures[5];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1278;
      res_op->ops[1] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 668, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1278:;
  return false;
}

bool
gimple_simplify_439 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && wi::bit_and_not (get_nonzero_bits (captures[0]), wi::to_wide (captures[2])) == 0
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1285;
      {
	tree tem;
	tem = captures[0];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 674, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1285:;
    }
  return false;
}

bool
gimple_simplify_443 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (bitwise_inverted_equal_p (captures[0], captures[2], wascmp)
 || bitwise_inverted_equal_p (captures[1], captures[2], wascmp)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1289;
	  {
	    tree tem;
	    tem =  wascmp ? constant_boolean_node (false, type) : build_zero_cst (type);
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 678, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1289:;
	}
  }
  return false;
}

bool
gimple_simplify_446 (gimple_match_op *res_op, gimple_seq *seq,
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1292;
	  {
	    res_op->set_op (COND_EXPR, type, 3);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] = captures[3];
	    res_op->ops[2] =  build_zero_cst (type);
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 680, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1292:;
	}
    }
  return false;
}

bool
gimple_simplify_454 (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1307;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 692, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1307:;
	    }
	  else
	    {
	      if (code1 == EQ_EXPR && !val
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1308;
		  {
		    tree tem;
		    tem =  constant_boolean_node (false, type);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 693, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1308:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1309;
		      {
			tree tem;
			tem = captures[3];
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 694, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1309:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1310;
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
				  if (!_r1) goto next_after_fail1310;
			        }
			      else
			        _r1 = _o1[0];
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 695, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1310:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1311;
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
				      if (!_r1) goto next_after_fail1311;
				    }
				  else
				    _r1 = _o1[0];
				  res_op->ops[1] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 696, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1311:;
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1312;
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
					  if (!_r1) goto next_after_fail1312;
				        }
				      else
				        _r1 = _o1[0];
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 697, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1312:;
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
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1313;
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
					      if (!_r1) goto next_after_fail1313;
					    }
					  else
					    _r1 = _o1[0];
					  res_op->ops[1] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 698, __FILE__, __LINE__, true);
					return true;
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
  return false;
}

bool
gimple_simplify_472 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1363;
  {
    res_op->set_op (LTGT_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 712, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1363:;
  return false;
}

bool
gimple_simplify_477 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1369;
  {
    res_op->set_op (BIT_IOR_EXPR, type, 2);
    res_op->ops[0] = captures[1];
    {
      tree _o1[1], _r1;
      _o1[0] = captures[2];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1369;
      res_op->ops[1] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 717, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1369:;
  return false;
}

bool
gimple_simplify_480 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1372;
  {
    res_op->set_op (MINUS_EXPR, type, 2);
    res_op->ops[0] = captures[0];
    res_op->ops[1] = captures[1];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 720, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1372:;
  return false;
}

bool
gimple_simplify_482 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
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
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1374;
	  {
	    res_op->set_op (NOP_EXPR, type, 1);
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[3];
		if (shift_type != TREE_TYPE (_o2[0])
		    && !useless_type_conversion_p (shift_type, TREE_TYPE (_o2[0])))
		  {
		    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, shift_type, _o2[0]);
		    tem_op.resimplify (lseq, valueize);
		    _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r2) goto next_after_fail1374;
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] = captures[4];
	      gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1374;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 722, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1374:;
      }
    }
  return false;
}

bool
gimple_simplify_491 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1384;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 730, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1384:;
  return false;
}

bool
gimple_simplify_494 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1387;
  {
    tree tem;
    tem = captures[1];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 732, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1387:;
  return false;
}

bool
gimple_simplify_500 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (maxmin))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[0])
          || !single_use (captures[2])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1405;
  {
    res_op->set_op (BIT_NOT_EXPR, type, 1);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[3];
      gimple_match_op tem_op (res_op->cond.any_else (), maxmin, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1405;
      res_op->ops[0] = _r1;
    }
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 738, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1405:;
  return false;
}

bool
gimple_simplify_506 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (FMIN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_finite_math_only
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1411;
      {
	res_op->set_op (MIN_EXPR, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 744, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1411:;
    }
  return false;
}

bool
gimple_simplify_511 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1416;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 749, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1416:;
  return false;
}

bool
gimple_simplify_518 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && bitwise_equal_p (captures[0], captures[2])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1423;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] = captures[1];
	  gimple_match_op tem_op (res_op->cond.any_else (), EQ_EXPR, boolean_type_node, _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1423;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 756, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1423:;
    }
  return false;
}

bool
gimple_simplify_525 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree_code code = minmax_from_comparison (cmp, captures[0], captures[3], captures[0], captures[1]);
      if (code == MIN_EXPR
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1477;
	  {
	    res_op->set_op (minmax, type, 2);
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[0];
	      _o1[1] = captures[3];
	      gimple_match_op tem_op (res_op->cond.any_else (), MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1477;
	      res_op->ops[0] = _r1;
	    }
	    res_op->ops[1] = captures[2];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 774, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1477:;
	}
      else
	{
	  if (code == MAX_EXPR
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1478;
	      {
		res_op->set_op (minmax, type, 2);
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[3];
		  gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1478;
		  res_op->ops[0] = _r1;
		}
		res_op->ops[1] = captures[2];
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 775, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1478:;
	    }
	}
  }
  return false;
}

bool
gimple_simplify_528 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1483;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 780, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1483:;
  return false;
}

bool
gimple_simplify_529 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1484;
  {
    tree tem;
    tem = captures[2];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 781, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1484:;
  return false;
}

bool
gimple_simplify_531 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1486;
      {
	tree tem;
	tem = captures[2];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 783, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1486:;
    }
  return false;
}

bool
gimple_simplify_533 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (type)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1488;
      {
	tree tem;
	tem = captures[2];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 783, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1488:;
    }
  return false;
}

bool
gimple_simplify_535 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::to_wide (uniform_integer_cst_p (captures[3])) == 2
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1490;
      {
	tree tem;
	tem = captures[2];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 784, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1490:;
    }
  return false;
}

bool
gimple_simplify_538 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::to_wide (uniform_integer_cst_p (captures[3])) == 2
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1493;
      {
	tree tem;
	tem = captures[2];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 784, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1493:;
    }
  return false;
}

bool
gimple_simplify_541 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::abs (wi::to_wide (captures[1])) == wi::to_wide (captures[3])
)
    {
      if (ABS_EXPR != ABSU_EXPR && wi::only_sign_bit_p (wi::to_wide (captures[1]))
)
	{
	  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[0]));
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1496;
	      {
		res_op->set_op (NOP_EXPR, type, 1);
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[0];
		  gimple_match_op tem_op (res_op->cond.any_else (), ABSU_EXPR, utype, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1496;
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 787, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1496:;
	  }
	}
      else
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1497;
	  {
	    tree tem;
	    tem = captures[2];
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 788, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1497:;
	}
    }
  return false;
}

bool
gimple_simplify_550 (gimple_match_op *res_op, gimple_seq *seq,
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1507;
      {
	tree tem;
	tem = captures[2];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 792, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1507:;
    }
  return false;
}

bool
gimple_simplify_558 (gimple_match_op *res_op, gimple_seq *seq,
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1516;
      {
	tree tem;
	tem = captures[1];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 796, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1516:;
    }
  return false;
}

bool
gimple_simplify_566 (gimple_match_op *res_op, gimple_seq *seq,
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1530;
		  {
		    res_op->set_op (ABSU_EXPR, type, 1);
		    res_op->ops[0] = captures[0];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 802, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1530:;
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1531;
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
			  if (!_r1) goto next_after_fail1531;
		        }
		      else
		        _r1 = _o1[0];
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 803, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1531:;
		}
	  }
	}
      else
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1532;
	  {
	    res_op->set_op (ABS_EXPR, type, 1);
	    res_op->ops[0] = captures[2];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 804, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1532:;
	}
    }
  return false;
}

bool
gimple_simplify_573 (gimple_match_op *res_op, gimple_seq *seq,
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1548;
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
			if (!_r2) goto next_after_fail1548;
			_o1[0] = _r2;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1548;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 810, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1548:;
		}
	      else
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1549;
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
			      if (!_r3) goto next_after_fail1549;
			    }
			  else
			    _r3 = _o3[0];
			  _o2[0] = _r3;
			}
			gimple_match_op tem_op (res_op->cond.any_else (), ABSU_EXPR, utype, _o2[0]);
			tem_op.resimplify (lseq, valueize);
			_r2 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r2) goto next_after_fail1549;
			_o1[0] = _r2;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r1) goto next_after_fail1549;
		      res_op->ops[0] = _r1;
		    }
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 811, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail1549:;
		}
	  }
	}
      else
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1550;
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
		if (!_r2) goto next_after_fail1550;
		_o1[0] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1550;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 812, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1550:;
	}
    }
  return false;
}

bool
gimple_simplify_591 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1573;
  {
    tree tem;
    tem = captures[0];
    res_op->set_value (tem);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 830, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1573:;
  return false;
}

bool
gimple_simplify_595 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (MINMAX))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (VECTOR_INTEGER_TYPE_P (type)
 && target_supports_op_p (type, MINMAX, optab_vector)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1577;
      {
	res_op->set_op (minmax, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[1];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 834, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1577:;
    }
  return false;
}

bool
gimple_simplify_603 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (uncond_op),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[2]);
      if (vectorized_internal_fn_supported_p (as_internal_fn (cond_op), op_type)
 && is_truth_type_for (op_type, TREE_TYPE (captures[0]))
 && single_use (captures[2])
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1585;
	  {
	    res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
	    {
	      tree _o1[5], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[0];
		gimple_match_op tem_op (res_op->cond.any_else (), BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1585;
		_o1[0] = _r2;
	      }
	      _o1[1] = captures[3];
	      _o1[2] = captures[4];
	      _o1[3] = captures[5];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		gimple_match_op tem_op (res_op->cond.any_else (), VIEW_CONVERT_EXPR, op_type, _o2[0]);
		tem_op.resimplify (lseq, valueize);
		_r2 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r2) goto next_after_fail1585;
		_o1[4] = _r2;
	      }
	      gimple_match_op tem_op (res_op->cond.any_else (), cond_op, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2], _o1[3], _o1[4]);
	      tem_op.resimplify (lseq, valueize);
	      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	      if (!_r1) goto next_after_fail1585;
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 837, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1585:;
	}
  }
  return false;
}

bool
gimple_simplify_612 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  gimple_seq *lseq = seq;
  if (lseq
      && (!single_use (captures[1])))
    lseq = NULL;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1594;
  {
    res_op->set_op (cond_op, type, 4);
    {
      tree _o1[2], _r1;
      _o1[0] = captures[2];
      _o1[1] = captures[0];
      gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      tem_op.resimplify (lseq, valueize);
      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
      if (!_r1) goto next_after_fail1594;
      res_op->ops[0] = _r1;
    }
    res_op->ops[1] = captures[3];
    res_op->ops[2] = captures[4];
    res_op->ops[3] = captures[5];
    res_op->resimplify (lseq, valueize);
    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 846, __FILE__, __LINE__, true);
    return true;
  }
next_after_fail1594:;
  return false;
}

bool
gimple_simplify_618 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ovf))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_PRECISION (TREE_TYPE (captures[1])) > TYPE_PRECISION (TREE_TYPE (captures[2]))
 && (!TYPE_UNSIGNED (TREE_TYPE (captures[1])) || TYPE_UNSIGNED (TREE_TYPE (captures[2])))
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1600;
      {
	res_op->set_op (ovf, type, 2);
	res_op->ops[0] = captures[0];
	res_op->ops[1] = captures[2];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 852, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1600:;
    }
  return false;
}

bool
gimple_simplify_627 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (froms),
 const combined_fn ARG_UNUSED (tos))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (optimize && canonicalize_math_p ()
 && targetm.libc_has_function (function_c99_misc, NULL_TREE)
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1609;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  gimple_match_op tem_op (res_op->cond.any_else (), tos, TREE_TYPE (_o1[0]), _o1[0]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1609;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 861, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1609:;
    }
  return false;
}

bool
gimple_simplify_634 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (floors))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_p ()
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1616;
      {
	res_op->set_op (FIX_TRUNC_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1616:;
    }
  return false;
}

bool
gimple_simplify_640 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SIGNBIT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGNED_ZEROS (captures[0])
)
    {
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1622;
      {
	res_op->set_op (NOP_EXPR, type, 1);
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
	  gimple_match_op tem_op (res_op->cond.any_else (), LT_EXPR, boolean_type_node, _o1[0], _o1[1]);
	  tem_op.resimplify (lseq, valueize);
	  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
	  if (!_r1) goto next_after_fail1622;
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 874, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1622:;
    }
  return false;
}

bool
gimple_simplify_650 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree t = TREE_TYPE (captures[0]);
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1632;
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
	      if (!_r1) goto next_after_fail1632;
	    }
	  else
	    _r1 = _o1[0];
	  res_op->ops[0] = _r1;
	}
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 884, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1632:;
  }
  return false;
}

bool
gimple_simplify_658 (gimple_match_op *res_op, gimple_seq *seq,
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
	          && (!single_use (captures[0])
	              || !single_use (captures[1])))
	        lseq = NULL;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1644;
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
			if (!_r2) goto next_after_fail1644;
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
		      if (!_r1) goto next_after_fail1644;
		    }
		  else
		    _r1 = _o1[0];
		  res_op->ops[0] = _r1;
		}
		res_op->resimplify (lseq, valueize);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 888, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail1644:;
	    }
      }
    }
  return false;
}

bool
gimple_simplify_666 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rot),
 const combined_fn ARG_UNUSED (parity))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && (
1
 || !TREE_SIDE_EFFECTS (captures[3]))
 && TYPE_PRECISION (TREE_TYPE (captures[0]))
 >= TYPE_PRECISION (TREE_TYPE (captures[1]))
)
    {
      {
 tree type0 = TREE_TYPE (captures[0]);
	  gimple_seq *lseq = seq;
	  if (lseq
	      && (!single_use (captures[0])
	          || !single_use (captures[1])))
	    lseq = NULL;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1655;
	  {
	    res_op->set_op (parity, type, 1);
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      if (type0 != TREE_TYPE (_o1[0])
	          && !useless_type_conversion_p (type0, TREE_TYPE (_o1[0])))
		{
		  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type0, _o1[0]);
		  tem_op.resimplify (lseq, valueize);
		  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		  if (!_r1) goto next_after_fail1655;
	        }
	      else
	        _r1 = _o1[0];
	      res_op->ops[0] = _r1;
	    }
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 897, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1655:;
      }
    }
  return false;
}

bool
gimple_simplify_672 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 const tree ARG_UNUSED (type), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (reduc))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree ctor = (TREE_CODE (captures[0]) == SSA_NAME
 ? gimple_assign_rhs1 (SSA_NAME_DEF_STMT (captures[0])) : captures[0]);
 tree elt = ctor_single_nonzero_element (ctor);
      if (elt
 && !HONOR_SNANS (type)
 && !HONOR_SIGNED_ZEROS (type)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1663;
	  {
	    tree tem;
	    tem =  elt;
	    res_op->set_value (tem);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 908, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail1663:;
	}
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_COSF (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn coss = CFN_BUILT_IN_COSF;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1694;
		    {
		      res_op->set_op (coss, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 932, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1694:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    const combined_fn coss = CFN_BUILT_IN_COSF;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1695;
		    {
		      res_op->set_op (coss, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 932, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1695:;
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_BUILT_IN_COSF))
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_BUILT_IN_COSF))
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_BUILT_IN_COSF))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_ATANF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		      if (gimple_simplify_410 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COSF, CFN_BUILT_IN_ATANF, CFN_BUILT_IN_SQRTF, CFN_BUILT_IN_COPYSIGNF))
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_BUILT_IN_COSF))
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
gimple_simplify_CFN_BUILT_IN_COSHL (gimple_match_op *res_op, gimple_seq *seq,
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
		    const combined_fn coss = CFN_BUILT_IN_COSHL;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1706;
		    {
		      res_op->set_op (coss, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 932, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1706:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    const combined_fn coss = CFN_BUILT_IN_COSHL;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1707;
		    {
		      res_op->set_op (coss, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 932, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1707:;
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_BUILT_IN_COSHL))
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_BUILT_IN_COSHL))
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_BUILT_IN_COSHL))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_ATANHL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		      if (gimple_simplify_411 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COSHL, CFN_BUILT_IN_ATANHL, CFN_BUILT_IN_SQRTL))
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
		      if (gimple_simplify_409 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_BUILT_IN_COSHL))
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
gimple_simplify_CFN_BUILT_IN_CABSL (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1739;
		    {
		      res_op->set_op (CFN_BUILT_IN_CABSL, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 945, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1739:;
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
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1740;
		    {
		      res_op->set_op (CFN_BUILT_IN_CABSL, type, 1);
		      res_op->ops[0] = captures[0];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 945, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1740:;
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1741;
			{
			  res_op->set_op (ABS_EXPR, type, 1);
			  res_op->ops[0] = captures[0];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 946, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail1741:;
		      }
		    }
		  if (real_zerop (_q20))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			gimple_seq *lseq = seq;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1742;
			{
			  res_op->set_op (ABS_EXPR, type, 1);
			  res_op->ops[0] = captures[0];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 946, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail1742:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1743;
			    {
			      res_op->set_op (MULT_EXPR, type, 2);
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[0];
				gimple_match_op tem_op (res_op->cond.any_else (), ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail1743;
				res_op->ops[0] = _r1;
			      }
			      res_op->ops[1] =  build_real_truncate (type, dconst_sqrt2 ());
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 947, __FILE__, __LINE__, true);
			      return true;
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
    default:;
    }
  return false;
}

bool
gimple_simplify_PAREN_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
	      case PAREN_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1794;
		    {
		      tree tem;
		      tem = captures[0];
		      res_op->set_value (tem);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 965, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1794:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
if (CONSTANT_CLASS_P (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      gimple_seq *lseq = seq;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1795;
      {
	tree tem;
	tem = captures[0];
	res_op->set_value (tem);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 966, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail1795:;
    }
  }
  return false;
}

bool
gimple_simplify_IMAGPART_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
	      case COMPLEX_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1809;
		    {
		      tree tem;
		      tem = captures[1];
		      res_op->set_value (tem);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 971, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1809:;
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
			      case CONJ_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
				    gimple_seq *lseq = seq;
				    if (lseq
				        && (!single_use (captures[0])
				            || !single_use (captures[1])))
				      lseq = NULL;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1810;
				    {
				      res_op->set_op (NOP_EXPR, type, 1);
				      {
					tree _o1[1], _r1;
					{
					  tree _o2[1], _r2;
					  _o2[0] = captures[2];
					  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r2) goto next_after_fail1810;
					  _o1[0] = _r2;
					}
					gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					tem_op.resimplify (lseq, valueize);
					_r1 = maybe_push_res_to_seq (&tem_op, lseq);
					if (!_r1) goto next_after_fail1810;
					res_op->ops[0] = _r1;
				      }
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 972, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail1810:;
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
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
				    const enum tree_code op = PLUS_EXPR;
				    gimple_seq *lseq = seq;
				    if (lseq
				        && (!single_use (captures[0])
				            || !single_use (captures[1])))
				      lseq = NULL;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1811;
				    {
				      res_op->set_op (NOP_EXPR, type, 1);
				      {
					tree _o1[2], _r1;
					{
					  tree _o2[1], _r2;
					  _o2[0] = captures[2];
					  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r2) goto next_after_fail1811;
					  _o1[0] = _r2;
					}
					{
					  tree _o2[1], _r2;
					  _o2[0] = captures[3];
					  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r2) goto next_after_fail1811;
					  _o1[1] = _r2;
					}
					gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					tem_op.resimplify (lseq, valueize);
					_r1 = maybe_push_res_to_seq (&tem_op, lseq);
					if (!_r1) goto next_after_fail1811;
					res_op->ops[0] = _r1;
				      }
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 969, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail1811:;
				  }
			          break;
			        }
			      case MINUS_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
				    const enum tree_code op = MINUS_EXPR;
				    gimple_seq *lseq = seq;
				    if (lseq
				        && (!single_use (captures[0])
				            || !single_use (captures[1])))
				      lseq = NULL;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1812;
				    {
				      res_op->set_op (NOP_EXPR, type, 1);
				      {
					tree _o1[2], _r1;
					{
					  tree _o2[1], _r2;
					  _o2[0] = captures[2];
					  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r2) goto next_after_fail1812;
					  _o1[0] = _r2;
					}
					{
					  tree _o2[1], _r2;
					  _o2[0] = captures[3];
					  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r2) goto next_after_fail1812;
					  _o1[1] = _r2;
					}
					gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					tem_op.resimplify (lseq, valueize);
					_r1 = maybe_push_res_to_seq (&tem_op, lseq);
					if (!_r1) goto next_after_fail1812;
					res_op->ops[0] = _r1;
				      }
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 969, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail1812:;
				  }
			          break;
			        }
			      default:;
			      }
			  else if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_CEXPIF:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
				      gimple_seq *lseq = seq;
				      if (lseq
				          && (!single_use (captures[0])
				              || !single_use (captures[1])))
				        lseq = NULL;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1813;
				      {
					res_op->set_op (NOP_EXPR, type, 1);
					{
					  tree _o1[1], _r1;
					  _o1[0] = captures[2];
					  gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_SINF, TREE_TYPE (_o1[0]), _o1[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail1813;
					  res_op->ops[0] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1813:;
				    }
			          }
			        break;
			      case CFN_BUILT_IN_CEXPIL:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
				      gimple_seq *lseq = seq;
				      if (lseq
				          && (!single_use (captures[0])
				              || !single_use (captures[1])))
				        lseq = NULL;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1814;
				      {
					res_op->set_op (NOP_EXPR, type, 1);
					{
					  tree _o1[1], _r1;
					  _o1[0] = captures[2];
					  gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_SINL, TREE_TYPE (_o1[0]), _o1[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail1814;
					  res_op->ops[0] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1814:;
				    }
			          }
			        break;
			      case CFN_BUILT_IN_CEXPI:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
				      gimple_seq *lseq = seq;
				      if (lseq
				          && (!single_use (captures[0])
				              || !single_use (captures[1])))
				        lseq = NULL;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1815;
				      {
					res_op->set_op (NOP_EXPR, type, 1);
					{
					  tree _o1[1], _r1;
					  _o1[0] = captures[2];
					  gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_SIN, TREE_TYPE (_o1[0]), _o1[0]);
					  tem_op.resimplify (lseq, valueize);
					  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					  if (!_r1) goto next_after_fail1815;
					  res_op->ops[0] = _r1;
					}
					res_op->resimplify (lseq, valueize);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail1815:;
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
	      case CONJ_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		    gimple_seq *lseq = seq;
		    if (lseq
		        && (!single_use (captures[0])
		            || !single_use (captures[1])))
		      lseq = NULL;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1816;
		    {
		      res_op->set_op (NOP_EXPR, type, 1);
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r2) goto next_after_fail1816;
			  _o1[0] = _r2;
			}
			gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
			tem_op.resimplify (lseq, valueize);
			_r1 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r1) goto next_after_fail1816;
			res_op->ops[0] = _r1;
		      }
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 972, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1816:;
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
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
		    const enum tree_code op = PLUS_EXPR;
		    gimple_seq *lseq = seq;
		    if (lseq
		        && (!single_use (captures[0])
		            || !single_use (captures[1])))
		      lseq = NULL;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1817;
		    {
		      res_op->set_op (NOP_EXPR, type, 1);
		      {
			tree _o1[2], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r2) goto next_after_fail1817;
			  _o1[0] = _r2;
			}
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[3];
			  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r2) goto next_after_fail1817;
			  _o1[1] = _r2;
			}
			gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			tem_op.resimplify (lseq, valueize);
			_r1 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r1) goto next_after_fail1817;
			res_op->ops[0] = _r1;
		      }
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 969, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1817:;
		  }
	          break;
	        }
	      case MINUS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
		    const enum tree_code op = MINUS_EXPR;
		    gimple_seq *lseq = seq;
		    if (lseq
		        && (!single_use (captures[0])
		            || !single_use (captures[1])))
		      lseq = NULL;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1818;
		    {
		      res_op->set_op (NOP_EXPR, type, 1);
		      {
			tree _o1[2], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r2) goto next_after_fail1818;
			  _o1[0] = _r2;
			}
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[3];
			  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r2) goto next_after_fail1818;
			  _o1[1] = _r2;
			}
			gimple_match_op tem_op (res_op->cond.any_else (), op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			tem_op.resimplify (lseq, valueize);
			_r1 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r1) goto next_after_fail1818;
			res_op->ops[0] = _r1;
		      }
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 969, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail1818:;
		  }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_MUL_OVERFLOW:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    if (integer_nonzerop (_q21))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			  if (gimple_simplify_589 (res_op, seq, valueize, type, captures))
			    return true;
			}
		      }
		    if (integer_nonzerop (_q20))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			  if (gimple_simplify_589 (res_op, seq, valueize, type, captures))
			    return true;
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_CEXPIF:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		      gimple_seq *lseq = seq;
		      if (lseq
		          && (!single_use (captures[0])
		              || !single_use (captures[1])))
		        lseq = NULL;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1819;
		      {
			res_op->set_op (NOP_EXPR, type, 1);
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[2];
			  gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_SINF, TREE_TYPE (_o1[0]), _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail1819;
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1819:;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CEXPIL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		      gimple_seq *lseq = seq;
		      if (lseq
		          && (!single_use (captures[0])
		              || !single_use (captures[1])))
		        lseq = NULL;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1820;
		      {
			res_op->set_op (NOP_EXPR, type, 1);
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[2];
			  gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_SINL, TREE_TYPE (_o1[0]), _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail1820;
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1820:;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CEXPI:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		      gimple_seq *lseq = seq;
		      if (lseq
		          && (!single_use (captures[0])
		              || !single_use (captures[1])))
		        lseq = NULL;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1821;
		      {
			res_op->set_op (NOP_EXPR, type, 1);
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[2];
			  gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_SIN, TREE_TYPE (_o1[0]), _o1[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r1) goto next_after_fail1821;
			  res_op->ops[0] = _r1;
			}
			res_op->resimplify (lseq, valueize);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail1821:;
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
gimple_simplify_CFN_BUILT_IN_LOGL (gimple_match_op *res_op, gimple_seq *seq,
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
			  if (gimple_simplify_620 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOGL))
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
		      if (gimple_simplify_619 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOGL))
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1836;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGL, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1836;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1836:;
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1837;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGL, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1837;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1837:;
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
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1838;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 977, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1838:;
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
		      if (gimple_simplify_621 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOGL, CFN_BUILT_IN_POWL))
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1839;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGL, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1839;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1839:;
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1840;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGL, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1840;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1840:;
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
			  if (SCALAR_FLOAT_TYPE_P (type)
)
			    {
			      {
 tree x;
 switch (CFN_BUILT_IN_EXP2L)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1841;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOGL, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1841;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1841:;
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
gimple_simplify_CFN_BUILT_IN_LOG10 (gimple_match_op *res_op, gimple_seq *seq,
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
			  if (gimple_simplify_620 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG10))
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
		      if (gimple_simplify_619 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG10))
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
	      case CFN_BUILT_IN_EXP:
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
 switch (CFN_BUILT_IN_EXP)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1874;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOG10, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1874;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1874:;
			      }
			    }
			}
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
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		      if (gimple_simplify_621 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG10, CFN_BUILT_IN_POW))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CBRT:
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
 switch (CFN_BUILT_IN_CBRT)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1875;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOG10, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1875;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1875:;
			      }
			    }
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP2:
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
 switch (CFN_BUILT_IN_EXP2)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1876;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    {
				      tree _o1[1], _r1;
				      _o1[0] =  x;
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOG10, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1876;
				      res_op->ops[0] = _r1;
				    }
				    res_op->ops[1] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1876:;
			      }
			    }
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_SQRT:
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
 switch (CFN_BUILT_IN_SQRT)
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
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1877;
				  {
				    res_op->set_op (MULT_EXPR, type, 2);
				    res_op->ops[0] =  x;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[0];
				      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_LOG10, TREE_TYPE (_o1[0]), _o1[0]);
				      tem_op.resimplify (lseq, valueize);
				      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				      if (!_r1) goto next_after_fail1877;
				      res_op->ops[1] = _r1;
				    }
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1877:;
			      }
			    }
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP10:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1878;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 977, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1878:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POW10:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1879;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 977, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1879:;
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
gimple_simplify_CFN_BUILT_IN_POW10 (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_LOG10:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1903;
			  {
			    tree tem;
			    tem = captures[0];
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1903:;
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
gimple_simplify_CFN_BUILT_IN_CBRT (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_EXP:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1938;
			  {
			    res_op->set_op (CFN_BUILT_IN_EXP, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real_truncate (type, dconst_third ());
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1938;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1938:;
			}
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
		    if (tree_expr_nonnegative_p (_q20))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1939;
			      {
				res_op->set_op (CFN_BUILT_IN_POW, type, 2);
				res_op->ops[0] = captures[0];
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[1];
				  _o1[1] =  build_real_truncate (type, dconst_third ());
				  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail1939;
				  res_op->ops[1] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 985, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1939:;
			    }
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_CBRT:
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1940;
			      {
				res_op->set_op (CFN_BUILT_IN_POW, type, 2);
				res_op->ops[0] = captures[0];
				res_op->ops[1] =  build_real_truncate (type, dconst_ninth ());
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 986, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1940:;
			    }
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_EXP2:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1941;
			  {
			    res_op->set_op (CFN_BUILT_IN_EXP2, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real_truncate (type, dconst_third ());
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1941;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1941:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_SQRT:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1942;
			  {
			    res_op->set_op (CFN_BUILT_IN_POW, type, 2);
			    res_op->ops[0] = captures[0];
			    res_op->ops[1] =  build_real_truncate (type, dconst_sixth ());
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 983, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1942:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP10:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1943;
			  {
			    res_op->set_op (CFN_BUILT_IN_EXP10, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real_truncate (type, dconst_third ());
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1943;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1943:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POW10:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1944;
			  {
			    res_op->set_op (CFN_BUILT_IN_POW10, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] =  build_real_truncate (type, dconst_third ());
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1944;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1944:;
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
gimple_simplify_CFN_SIN (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_ATAN:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		      if (gimple_simplify_623 (res_op, seq, valueize, type, captures, CFN_SIN, CFN_ATAN, CFN_SQRT, CFN_COPYSIGN))
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
gimple_simplify_CFN_BUILT_IN_TRUNCL (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_628 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCL, CFN_BUILT_IN_TRUNC))
			  return true;
		      }
		    }
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_627 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCL, CFN_BUILT_IN_TRUNCF))
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
	      case CFN_BUILT_IN_TRUNCL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCL))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_TRUNCL))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FLOOR (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_627 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOOR, CFN_BUILT_IN_FLOORF))
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
	      case CFN_BUILT_IN_FLOOR:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOOR))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOOR))
        return true;
    }
  }
if (tree_expr_nonnegative_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_629 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOOR, CFN_BUILT_IN_TRUNC))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FLOORF128X (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_FLOORF128X:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF128X))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF128X))
        return true;
    }
  }
if (tree_expr_nonnegative_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (gimple_simplify_629 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FLOORF128X, CFN_BUILT_IN_TRUNCF128X))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_CEILF128 (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_CEILF128:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILF128))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_CEILF128))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_ROUNDL (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_628 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDL, CFN_BUILT_IN_ROUND))
			  return true;
		      }
		    }
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_627 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDL, CFN_BUILT_IN_ROUNDF))
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
	      case CFN_BUILT_IN_ROUNDL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDL))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ROUNDL))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_NEARBYINT (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_627 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINT, CFN_BUILT_IN_NEARBYINTF))
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
	      case CFN_BUILT_IN_NEARBYINT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINT))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_NEARBYINT))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_NEARBYINT (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_NEARBYINT:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_NEARBYINT))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_NEARBYINT))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_RINTF128 (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_BUILT_IN_RINTF128:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		      if (gimple_simplify_625 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTF128))
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
      if (gimple_simplify_626 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_RINTF128))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_CEXPL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (gimple_compositional_complex (_p0, valueize))
    {
      {
	tree captures[3] ATTRIBUTE_UNUSED = { _p0 };
	if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
	  {
	    if (targetm.libc_has_function (function_c99_math_complex, TREE_TYPE (captures[0]))
)
	      {
		gimple_seq *lseq = seq;
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1954;
		{
		  res_op->set_op (COMPLEX_EXPR, type, 2);
		  {
		    tree _o1[2], _r1;
		    {
		      tree _o2[1], _r2;
		      {
			tree _o3[1], _r3;
			_o3[0] = captures[0];
			gimple_match_op tem_op (res_op->cond.any_else (), REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o3[0])), _o3[0]);
			tem_op.resimplify (lseq, valueize);
			_r3 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r3) goto next_after_fail1954;
			_o2[0] = _r3;
		      }
		      gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_EXPL, TREE_TYPE (_o2[0]), _o2[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r2) goto next_after_fail1954;
		      captures[1] = _r2;
		    }
		    _o1[0] = captures[1];
		    {
		      tree _o2[1], _r2;
		      {
			tree _o3[1], _r3;
			{
			  tree _o4[1], _r4;
			  _o4[0] = captures[0];
			  gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o4[0])), _o4[0]);
			  tem_op.resimplify (lseq, valueize);
			  _r4 = maybe_push_res_to_seq (&tem_op, lseq);
			  if (!_r4) goto next_after_fail1954;
			  _o3[0] = _r4;
			}
			gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_CEXPIL, type, _o3[0]);
			tem_op.resimplify (lseq, valueize);
			_r3 = maybe_push_res_to_seq (&tem_op, lseq);
			if (!_r3) goto next_after_fail1954;
			captures[2] = _r3;
		      }
		      _o2[0] = captures[2];
		      gimple_match_op tem_op (res_op->cond.any_else (), REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r2) goto next_after_fail1954;
		      _o1[1] = _r2;
		    }
		    gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		    tem_op.resimplify (lseq, valueize);
		    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r1) goto next_after_fail1954;
		    res_op->ops[0] = _r1;
		  }
		  {
		    tree _o1[2], _r1;
		    _o1[0] = captures[1];
		    {
		      tree _o2[1], _r2;
		      _o2[0] = captures[2];
		      gimple_match_op tem_op (res_op->cond.any_else (), IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		      tem_op.resimplify (lseq, valueize);
		      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
		      if (!_r2) goto next_after_fail1954;
		      _o1[1] = _r2;
		    }
		    gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		    tem_op.resimplify (lseq, valueize);
		    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
		    if (!_r1) goto next_after_fail1954;
		    res_op->ops[1] = _r1;
		  }
		  res_op->resimplify (lseq, valueize);
		  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 987, __FILE__, __LINE__, true);
		  return true;
		}
next_after_fail1954:;
	      }
	  }
      }
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LRINTL (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_632 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LRINTL, CFN_BUILT_IN_LRINT))
			  return true;
		      }
		    }
		  if (gimple_float_value_p (_q20, valueize))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LRINTL, CFN_BUILT_IN_LRINTF))
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
      if (gimple_simplify_638 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LRINTL))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_ICEIL (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ICEIL, CFN_BUILT_IN_ICEILF))
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
      if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ICEIL))
        return true;
    }
  }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_636 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ICEIL, CFN_BUILT_IN_LCEIL, CFN_BUILT_IN_LLCEIL))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LLRINT (gimple_match_op *res_op, gimple_seq *seq,
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
			if (gimple_simplify_633 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LLRINT, CFN_BUILT_IN_LLRINTF))
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
      if (gimple_simplify_638 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LLRINT))
        return true;
    }
  }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_637 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_IRINT, CFN_BUILT_IN_LRINT, CFN_BUILT_IN_LLRINT))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LLCEILF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LLCEILF))
	  return true;
      }
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_637 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_ICEILF, CFN_BUILT_IN_LCEILF, CFN_BUILT_IN_LLCEILF))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_LLROUND (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_valued_real_p (_p0))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_635 (res_op, seq, valueize, type, captures, CFN_LLROUND))
	  return true;
      }
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (gimple_simplify_637 (res_op, seq, valueize, type, captures, CFN_IROUND, CFN_LROUND, CFN_LLROUND))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_CPROJF (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if (!HONOR_INFINITIES (type)
)
      {
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1955;
	{
	  tree tem;
	  tem = captures[0];
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 988, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail1955:;
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
	      case COMPLEX_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q20))
		    {
		    case REAL_CST:
		      {
			if (tree_expr_nonnegative_p (_q21))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      if (real_isinf (TREE_REAL_CST_PTR (captures[0]))
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1956;
				  {
				    tree tem;
				    tem =  build_complex_inf (type, false);
				    res_op->set_value (tem);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 989, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail1956:;
				}
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case REAL_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			  if (real_isinf (TREE_REAL_CST_PTR (captures[1]))
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1957;
			      {
				tree tem;
				tem =  build_complex_inf (type, TREE_REAL_CST_PTR (captures[1])->sign);
				res_op->set_value (tem);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 990, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail1957:;
			    }
			}
		        break;
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
gimple_simplify_CFN_CTZ (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_652 (res_op, seq, valueize, type, captures, CFN_CTZ))
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
		    if (gimple_simplify_653 (res_op, seq, valueize, type, captures, CFN_CTZ))
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
		    if (gimple_simplify_654 (res_op, seq, valueize, type, captures, CFN_CTZ))
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
			if (gimple_simplify_650 (res_op, seq, valueize, type, captures, CFN_CTZ))
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
			if (gimple_simplify_651 (res_op, seq, valueize, type, captures, CFN_CTZ))
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
gimple_simplify_CFN_BUILT_IN_PARITYL (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_661 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_PARITYL))
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
				    if (gimple_simplify_663 (res_op, seq, valueize, type, captures, LROTATE_EXPR, CFN_BUILT_IN_PARITYL))
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
				    if (gimple_simplify_663 (res_op, seq, valueize, type, captures, RROTATE_EXPR, CFN_BUILT_IN_PARITYL))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128, CFN_BUILT_IN_PARITYL))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16, CFN_BUILT_IN_PARITYL))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32, CFN_BUILT_IN_PARITYL))
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
				      if (gimple_simplify_662 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64, CFN_BUILT_IN_PARITYL))
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
		    if (gimple_simplify_664 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_PARITYL))
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
		    if (gimple_simplify_666 (res_op, seq, valueize, type, captures, LROTATE_EXPR, CFN_BUILT_IN_PARITYL))
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
		    if (gimple_simplify_666 (res_op, seq, valueize, type, captures, RROTATE_EXPR, CFN_BUILT_IN_PARITYL))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP128, CFN_BUILT_IN_PARITYL))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP16, CFN_BUILT_IN_PARITYL))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP32, CFN_BUILT_IN_PARITYL))
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
		      if (gimple_simplify_665 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_BSWAP64, CFN_BUILT_IN_PARITYL))
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
    if (gimple_simplify_660 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_PARITYL))
      return true;
  }
  return false;
}

bool
gimple_simplify_CFN_REDUC_MAX (gimple_match_op *res_op, gimple_seq *seq,
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
	      case MAX_EXPR:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1967;
			  {
			    res_op->set_op (MAX_EXPR, type, 2);
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[0];
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_MAX, type, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1967;
			      res_op->ops[0] = _r1;
			    }
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_MAX, type, _o1[0]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail1967;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 991, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail1967:;
			}
		        break;
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
gimple_simplify_CFN_REDUC_FMIN (gimple_match_op *res_op, gimple_seq *seq,
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
	      case CFN_FMIN:
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1970;
			    {
			      res_op->set_op (CFN_FMIN, type, 2);
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[0];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_FMIN, type, _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail1970;
				res_op->ops[0] = _r1;
			      }
			      {
				tree _o1[1], _r1;
				_o1[0] = captures[1];
				gimple_match_op tem_op (res_op->cond.any_else (), CFN_REDUC_FMIN, type, _o1[0]);
				tem_op.resimplify (lseq, valueize);
				_r1 = maybe_push_res_to_seq (&tem_op, lseq);
				if (!_r1) goto next_after_fail1970;
				res_op->ops[1] = _r1;
			      }
			      res_op->resimplify (lseq, valueize);
			      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 991, __FILE__, __LINE__, true);
			      return true;
			    }
next_after_fail1970:;
			  }
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
gimple_simplify_MINUS_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2062;
	{
	  tree tem;
	  tem = captures[0];
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 992, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail2062:;
      }
    }
  if (real_zerop (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (fold_real_zero_addition_p (type, captures[0], captures[1], 1)
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2063;
	    {
	      tree tem;
	      tem = captures[0];
	      res_op->set_value (tem);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1013, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2063:;
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_50 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR))
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
				if (gimple_simplify_50 (res_op, seq, valueize, type, captures, MINUS_EXPR, MINUS_EXPR))
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
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2064;
	    {
	      tree tem;
	      tem =  build_zero_cst (type);
	      res_op->set_value (tem);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1014, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2064:;
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
					      case MULT_EXPR:
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
							      case TRUNC_DIV_EXPR:
							        {
								  tree _q60 = gimple_assign_rhs1 (_a4);
								  _q60 = do_valueize (valueize, _q60);
								  tree _q61 = gimple_assign_rhs2 (_a4);
								  _q61 = do_valueize (valueize, _q61);
								  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q20, 0))
								    {
								      if ((_q51 == _q61 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q61, 0))
									{
									  {
									    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q61, _q20, _q61 };
									    if (gimple_simplify_83 (res_op, seq, valueize, type, captures))
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
						  switch (TREE_CODE (_q51))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q51))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case TRUNC_DIV_EXPR:
							        {
								  tree _q70 = gimple_assign_rhs1 (_a4);
								  _q70 = do_valueize (valueize, _q70);
								  tree _q71 = gimple_assign_rhs2 (_a4);
								  _q71 = do_valueize (valueize, _q71);
								  if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q20, 0))
								    {
								      if ((_q71 == _q50 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q50, 0))
									{
									  {
									    tree captures[4] ATTRIBUTE_UNUSED = { _q70, _q71, _q20, _q50 };
									    if (gimple_simplify_83 (res_op, seq, valueize, type, captures))
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
				  switch (TREE_CODE (_q40))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q40))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case TRUNC_DIV_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
						  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
						    {
						      if ((_q41 == _q51 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q51, 0))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _q50, _q51, _q20, _q51 };
							    if (gimple_simplify_83 (res_op, seq, valueize, type, captures))
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
				  switch (TREE_CODE (_q41))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q41))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case TRUNC_DIV_EXPR:
					        {
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  tree _q61 = gimple_assign_rhs2 (_a3);
						  _q61 = do_valueize (valueize, _q61);
						  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q20, 0))
						    {
						      if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || operand_equal_p (_q61, _q40, 0))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q61, _q20, _q40 };
							    if (gimple_simplify_83 (res_op, seq, valueize, type, captures))
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
			      case MULT_EXPR:
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
					      case TRUNC_DIV_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
						  if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _p0, 0))
						    {
						      if ((_q41 == _q51 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q51, 0))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _q50, _q51, _p0, _q51 };
							    if (gimple_simplify_83 (res_op, seq, valueize, type, captures))
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
				  switch (TREE_CODE (_q41))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q41))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case TRUNC_DIV_EXPR:
					        {
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  tree _q61 = gimple_assign_rhs2 (_a3);
						  _q61 = do_valueize (valueize, _q61);
						  if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _p0, 0))
						    {
						      if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || operand_equal_p (_q61, _q40, 0))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q61, _p0, _q40 };
							    if (gimple_simplify_83 (res_op, seq, valueize, type, captures))
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
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
				    {
				      if ((_q31 == _q41 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _q41, 0))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _p0, _q41 };
					    if (gimple_simplify_83 (res_op, seq, valueize, type, captures))
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
				  if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _p0, 0))
				    {
				      if ((_q51 == _q30 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q30, 0))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q50, _q51, _p0, _q30 };
					    if (gimple_simplify_83 (res_op, seq, valueize, type, captures))
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
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
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
					      case TRUNC_DIV_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
						  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
						    {
						      if (integer_onep (_q41))
							{
							  if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
							    {
							      {
								tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q50 };
								if (gimple_simplify_84 (res_op, seq, valueize, type, captures))
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
					      case TRUNC_DIV_EXPR:
					        {
						  tree _q40 = gimple_assign_rhs1 (_a3);
						  _q40 = do_valueize (valueize, _q40);
						  tree _q41 = gimple_assign_rhs2 (_a3);
						  _q41 = do_valueize (valueize, _q41);
						  if (integer_onep (_q31))
						    {
						      if ((_q21 == _q41 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q41, 0) && types_match (_q21, _q41)))
							{
							  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
							    {
							      {
								tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q20, _q30, _q40 };
								if (gimple_simplify_84 (res_op, seq, valueize, type, captures))
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
			          break;
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
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case LSHIFT_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
					if (gimple_simplify_52 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case BIT_NOT_EXPR:
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
					      case BIT_AND_EXPR:
					        {
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  tree _q61 = gimple_assign_rhs2 (_a3);
						  _q61 = do_valueize (valueize, _q61);
						  if (tree_swap_operands_p (_q60, _q61))
						    std::swap (_q60, _q61);
						  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
						    {
						      if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _p1 };
							    if (gimple_simplify_85 (res_op, seq, valueize, type, captures))
							      return true;
							  }
						        }
						    }
						  if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
						    {
						      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _p1 };
							    if (gimple_simplify_85 (res_op, seq, valueize, type, captures))
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case BIT_NOT_EXPR:
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
					      case BIT_AND_EXPR:
					        {
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  tree _q61 = gimple_assign_rhs2 (_a3);
						  _q61 = do_valueize (valueize, _q61);
						  if (tree_swap_operands_p (_q60, _q61))
						    std::swap (_q60, _q61);
						  if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
						    {
						      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q30, _p1 };
							    if (gimple_simplify_85 (res_op, seq, valueize, type, captures))
							      return true;
							  }
						        }
						    }
						  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
						    {
						      if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q30, _p1 };
							    if (gimple_simplify_85 (res_op, seq, valueize, type, captures))
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
					    switch (TREE_CODE (_q51))
					      {
					      case INTEGER_CST:
					        {
						  {
						    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
						    if (~wi::to_wide (captures[2]) == wi::to_wide (captures[4])
)
						      {
							gimple_seq *lseq = seq;
							if (lseq
							    && (!single_use (captures[0])
							        || !single_use (captures[3])))
							  lseq = NULL;
							if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2065;
							{
							  res_op->set_op (MINUS_EXPR, type, 2);
							  {
							    tree _o1[2], _r1;
							    _o1[0] = captures[1];
							    _o1[1] = captures[4];
							    gimple_match_op tem_op (res_op->cond.any_else (), BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							    tem_op.resimplify (lseq, valueize);
							    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
							    if (!_r1) goto next_after_fail2065;
							    res_op->ops[0] = _r1;
							  }
							  res_op->ops[1] = captures[4];
							  res_op->resimplify (lseq, valueize);
							  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1015, __FILE__, __LINE__, true);
							  return true;
							}
next_after_fail2065:;
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
				      switch (TREE_CODE (_q51))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _q51))
					    {
					      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					        switch (gimple_assign_rhs_code (_a3))
						  {
						  case BIT_NOT_EXPR:
						    {
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
							    if (gimple_simplify_86 (res_op, seq, valueize, type, captures))
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
				  switch (TREE_CODE (_q50))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q50))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case BIT_NOT_EXPR:
					        {
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
						    {
						      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
							    if (gimple_simplify_86 (res_op, seq, valueize, type, captures))
							      return true;
							  }
						        }
						    }
						  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
						    {
						      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1 };
							    if (gimple_simplify_86 (res_op, seq, valueize, type, captures))
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
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      switch (TREE_CODE (_q51))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _q51))
					    {
					      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					        switch (gimple_assign_rhs_code (_a3))
						  {
						  case BIT_NOT_EXPR:
						    {
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
							{
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1 };
							    if (gimple_simplify_86 (res_op, seq, valueize, type, captures))
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
			      case PLUS_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  switch (TREE_CODE (_q50))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q50))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case BIT_IOR_EXPR:
					        {
						  tree _q60 = gimple_assign_rhs1 (_a3);
						  _q60 = do_valueize (valueize, _q60);
						  tree _q61 = gimple_assign_rhs2 (_a3);
						  _q61 = do_valueize (valueize, _q61);
						  if (tree_swap_operands_p (_q60, _q61))
						    std::swap (_q60, _q61);
						  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
						    {
						      if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
							{
							  if (integer_onep (_q51))
							    {
							      {
								tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
								if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
								  return true;
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
								if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
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
				{
				  tree _q50_pops[1];
				  if (gimple_nop_convert (_q50, _q50_pops, valueize))
				    {
				      tree _q60 = _q50_pops[0];
				      switch (TREE_CODE (_q60))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _q60))
					    {
					      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					        switch (gimple_assign_rhs_code (_a3))
						  {
						  case BIT_IOR_EXPR:
						    {
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      tree _q71 = gimple_assign_rhs2 (_a3);
						      _q71 = do_valueize (valueize, _q71);
						      if (tree_swap_operands_p (_q70, _q71))
						        std::swap (_q70, _q71);
						      if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
							{
							  if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
							    {
							      if (integer_onep (_q51))
								{
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
								    if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
								      return true;
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
								    if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
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
				  case PLUS_EXPR:
				    {
				      tree _q60 = gimple_assign_rhs1 (_a2);
				      _q60 = do_valueize (valueize, _q60);
				      tree _q61 = gimple_assign_rhs2 (_a2);
				      _q61 = do_valueize (valueize, _q61);
				      if (tree_swap_operands_p (_q60, _q61))
				        std::swap (_q60, _q61);
				      switch (TREE_CODE (_q60))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _q60))
					    {
					      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					        switch (gimple_assign_rhs_code (_a3))
						  {
						  case BIT_IOR_EXPR:
						    {
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      tree _q71 = gimple_assign_rhs2 (_a3);
						      _q71 = do_valueize (valueize, _q71);
						      if (tree_swap_operands_p (_q70, _q71))
						        std::swap (_q70, _q71);
						      if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
							{
							  if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
							    {
							      if (integer_onep (_q61))
								{
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
								    if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
								      return true;
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
								    if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
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
				    {
				      tree _q60_pops[1];
				      if (gimple_nop_convert (_q60, _q60_pops, valueize))
				        {
					  tree _q70 = _q60_pops[0];
					  switch (TREE_CODE (_q70))
					    {
					    case SSA_NAME:
					      if (gimple *_d3 = get_def (valueize, _q70))
					        {
						  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						    switch (gimple_assign_rhs_code (_a3))
						      {
						      case BIT_IOR_EXPR:
						        {
							  tree _q80 = gimple_assign_rhs1 (_a3);
							  _q80 = do_valueize (valueize, _q80);
							  tree _q81 = gimple_assign_rhs2 (_a3);
							  _q81 = do_valueize (valueize, _q81);
							  if (tree_swap_operands_p (_q80, _q81))
							    std::swap (_q80, _q81);
							  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
							    {
							      if ((_q81 == _q21 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q21, 0) && types_match (_q81, _q21)))
								{
								  if (integer_onep (_q61))
								    {
								      {
									tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
									if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
									  return true;
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
									if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
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
				    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
				    if (!TYPE_OVERFLOW_SANITIZED (type)
)
				      {
					{
 tree utype = unsigned_type_for (type);
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2066;
					    {
					      res_op->set_op (NOP_EXPR, type, 1);
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
						      if (!_r2) goto next_after_fail2066;
						    }
						  else
						    _r2 = _o2[0];
						  _o1[0] = _r2;
						}
						{
						  tree _o2[1], _r2;
						  _o2[0] = captures[0];
						  if (utype != TREE_TYPE (_o2[0])
						      && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
						    {
						      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
						      tem_op.resimplify (lseq, valueize);
						      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
						      if (!_r2) goto next_after_fail2066;
						    }
						  else
						    _r2 = _o2[0];
						  _o1[1] = _r2;
						}
						gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (lseq, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, lseq);
						if (!_r1) goto next_after_fail2066;
						res_op->ops[0] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1016, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2066:;
					}
				      }
				  }
			          break;
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
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case BIT_IOR_EXPR:
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
					    if (!TYPE_OVERFLOW_SANITIZED (type) && !TYPE_OVERFLOW_TRAPS (type)
 && !TYPE_SATURATING (type)
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2067;
						{
						  res_op->set_op (BIT_AND_EXPR, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1017, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2067:;
					      }
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
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2068;
						{
						  res_op->set_op (BIT_IOR_EXPR, type, 2);
						  res_op->ops[0] = captures[0];
						  res_op->ops[1] = captures[1];
						  res_op->resimplify (lseq, valueize);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1018, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2068:;
					      }
					  }
				        }
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
			      case BIT_AND_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (tree_swap_operands_p (_q30, _q31))
				    std::swap (_q30, _q31);
				  if (integer_all_onesp (_q21))
				    {
				      switch (TREE_CODE (_p1))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _p1))
					    {
					      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					        switch (gimple_assign_rhs_code (_a3))
						  {
						  case BIT_IOR_EXPR:
						    {
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      tree _q71 = gimple_assign_rhs2 (_a3);
						      _q71 = do_valueize (valueize, _q71);
						      if (tree_swap_operands_p (_q70, _q71))
						        std::swap (_q70, _q71);
						      if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
							{
							  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
							    {
							      {
								tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
								if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
								  return true;
							      }
							    }
						        }
						      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
							{
							  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
							    {
							      {
								tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q31, _q30 };
								if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
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
				      if (gimple_nop_convert (_p1, _p1_pops, valueize))
				        {
					  tree _q70 = _p1_pops[0];
					  switch (TREE_CODE (_q70))
					    {
					    case SSA_NAME:
					      if (gimple *_d3 = get_def (valueize, _q70))
					        {
						  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						    switch (gimple_assign_rhs_code (_a3))
						      {
						      case BIT_IOR_EXPR:
						        {
							  tree _q80 = gimple_assign_rhs1 (_a3);
							  _q80 = do_valueize (valueize, _q80);
							  tree _q81 = gimple_assign_rhs2 (_a3);
							  _q81 = do_valueize (valueize, _q81);
							  if (tree_swap_operands_p (_q80, _q81))
							    std::swap (_q80, _q81);
							  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
							    {
							      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
								{
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
								    if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
								      return true;
								  }
							        }
							    }
							  if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
							    {
							      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
								{
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q31, _q30 };
								    if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
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
				        }
				    }
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
				  case BIT_AND_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      if (tree_swap_operands_p (_q40, _q41))
				        std::swap (_q40, _q41);
				      if (integer_all_onesp (_q21))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case SSA_NAME:
					      if (gimple *_d3 = get_def (valueize, _p1))
					        {
						  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						    switch (gimple_assign_rhs_code (_a3))
						      {
						      case BIT_IOR_EXPR:
						        {
							  tree _q80 = gimple_assign_rhs1 (_a3);
							  _q80 = do_valueize (valueize, _q80);
							  tree _q81 = gimple_assign_rhs2 (_a3);
							  _q81 = do_valueize (valueize, _q81);
							  if (tree_swap_operands_p (_q80, _q81))
							    std::swap (_q80, _q81);
							  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
							    {
							      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
								{
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q41 };
								    if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
								      return true;
								  }
							        }
							    }
							  if ((_q80 == _q41 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q41, 0) && types_match (_q80, _q41)))
							    {
							      if ((_q81 == _q40 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q40, 0) && types_match (_q81, _q40)))
								{
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q41, _q40 };
								    if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
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
					  if (gimple_nop_convert (_p1, _p1_pops, valueize))
					    {
					      tree _q80 = _p1_pops[0];
					      switch (TREE_CODE (_q80))
					        {
						case SSA_NAME:
						  if (gimple *_d3 = get_def (valueize, _q80))
						    {
						      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						        switch (gimple_assign_rhs_code (_a3))
							  {
							  case BIT_IOR_EXPR:
							    {
							      tree _q90 = gimple_assign_rhs1 (_a3);
							      _q90 = do_valueize (valueize, _q90);
							      tree _q91 = gimple_assign_rhs2 (_a3);
							      _q91 = do_valueize (valueize, _q91);
							      if (tree_swap_operands_p (_q90, _q91))
							        std::swap (_q90, _q91);
							      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
								{
								  if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
								    {
								      {
									tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q41 };
									if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
									  return true;
								      }
								    }
							        }
							      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
								{
								  if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
								    {
								      {
									tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q41, _q40 };
									if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
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
					    }
					}
				        }
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_89 (res_op, seq, valueize, type, captures))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			if (gimple_simplify_89 (res_op, seq, valueize, type, captures))
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2069;
					    {
					      res_op->set_op (BIT_AND_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[1];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1019, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2069:;
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
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2070;
					    {
					      res_op->set_op (BIT_XOR_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[1];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1020, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2070:;
					  }
				        }
				    }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
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
				  case BIT_AND_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      if (tree_swap_operands_p (_q40, _q41))
				        std::swap (_q40, _q41);
				      if (integer_all_onesp (_q31))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case SSA_NAME:
					      if (gimple *_d3 = get_def (valueize, _p1))
					        {
						  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						    switch (gimple_assign_rhs_code (_a3))
						      {
						      case BIT_IOR_EXPR:
						        {
							  tree _q80 = gimple_assign_rhs1 (_a3);
							  _q80 = do_valueize (valueize, _q80);
							  tree _q81 = gimple_assign_rhs2 (_a3);
							  _q81 = do_valueize (valueize, _q81);
							  if (tree_swap_operands_p (_q80, _q81))
							    std::swap (_q80, _q81);
							  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
							    {
							      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
								{
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q40, _q41 };
								    if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
								      return true;
								  }
							        }
							    }
							  if ((_q80 == _q41 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q41, 0) && types_match (_q80, _q41)))
							    {
							      if ((_q81 == _q40 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q40, 0) && types_match (_q81, _q40)))
								{
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q41, _q40 };
								    if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
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
					  if (gimple_nop_convert (_p1, _p1_pops, valueize))
					    {
					      tree _q80 = _p1_pops[0];
					      switch (TREE_CODE (_q80))
					        {
						case SSA_NAME:
						  if (gimple *_d3 = get_def (valueize, _q80))
						    {
						      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						        switch (gimple_assign_rhs_code (_a3))
							  {
							  case BIT_IOR_EXPR:
							    {
							      tree _q90 = gimple_assign_rhs1 (_a3);
							      _q90 = do_valueize (valueize, _q90);
							      tree _q91 = gimple_assign_rhs2 (_a3);
							      _q91 = do_valueize (valueize, _q91);
							      if (tree_swap_operands_p (_q90, _q91))
							        std::swap (_q90, _q91);
							      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
								{
								  if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
								    {
								      {
									tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q40, _q41 };
									if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
									  return true;
								      }
								    }
							        }
							      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
								{
								  if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
								    {
								      {
									tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q41, _q40 };
									if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
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
					    }
					}
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
		      if (gimple_nop_convert (_q30, _q30_pops, valueize))
		        {
			  tree _q40 = _q30_pops[0];
			  switch (TREE_CODE (_q40))
			    {
			    case SSA_NAME:
			      if (gimple *_d2 = get_def (valueize, _q40))
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
					  if (integer_all_onesp (_q31))
					    {
					      switch (TREE_CODE (_p1))
					        {
						case SSA_NAME:
						  if (gimple *_d3 = get_def (valueize, _p1))
						    {
						      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						        switch (gimple_assign_rhs_code (_a3))
							  {
							  case BIT_IOR_EXPR:
							    {
							      tree _q90 = gimple_assign_rhs1 (_a3);
							      _q90 = do_valueize (valueize, _q90);
							      tree _q91 = gimple_assign_rhs2 (_a3);
							      _q91 = do_valueize (valueize, _q91);
							      if (tree_swap_operands_p (_q90, _q91))
							        std::swap (_q90, _q91);
							      if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
								{
								  if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
								    {
								      {
									tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _q51 };
									if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
									  return true;
								      }
								    }
							        }
							      if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
								{
								  if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
								    {
								      {
									tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _q50 };
									if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
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
					      if (gimple_nop_convert (_p1, _p1_pops, valueize))
					        {
						  tree _q90 = _p1_pops[0];
						  switch (TREE_CODE (_q90))
						    {
						    case SSA_NAME:
						      if (gimple *_d3 = get_def (valueize, _q90))
						        {
							  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							    switch (gimple_assign_rhs_code (_a3))
							      {
							      case BIT_IOR_EXPR:
							        {
								  tree _q100 = gimple_assign_rhs1 (_a3);
								  _q100 = do_valueize (valueize, _q100);
								  tree _q101 = gimple_assign_rhs2 (_a3);
								  _q101 = do_valueize (valueize, _q101);
								  if (tree_swap_operands_p (_q100, _q101))
								    std::swap (_q100, _q101);
								  if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
								    {
								      if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
									{
									  {
									    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _q51 };
									    if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
									      return true;
									  }
								        }
								    }
								  if ((_q100 == _q51 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q51, 0) && types_match (_q100, _q51)))
								    {
								      if ((_q101 == _q50 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q50, 0) && types_match (_q101, _q50)))
									{
									  {
									    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _q50 };
									    if (gimple_simplify_88 (res_op, seq, valueize, type, captures))
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
					        }
					    }
					    }
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
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      tree _q31 = gimple_assign_rhs2 (_a1);
		      _q31 = do_valueize (valueize, _q31);
		      if (tree_swap_operands_p (_q30, _q31))
		        std::swap (_q30, _q31);
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
				      tree _q60 = gimple_assign_rhs1 (_a2);
				      _q60 = do_valueize (valueize, _q60);
				      tree _q61 = gimple_assign_rhs2 (_a2);
				      _q61 = do_valueize (valueize, _q61);
				      if (tree_swap_operands_p (_q60, _q61))
				        std::swap (_q60, _q61);
				      switch (TREE_CODE (_q60))
				        {
					case SSA_NAME:
					  if (gimple *_d3 = get_def (valueize, _q60))
					    {
					      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					        switch (gimple_assign_rhs_code (_a3))
						  {
						  case BIT_IOR_EXPR:
						    {
						      tree _q70 = gimple_assign_rhs1 (_a3);
						      _q70 = do_valueize (valueize, _q70);
						      tree _q71 = gimple_assign_rhs2 (_a3);
						      _q71 = do_valueize (valueize, _q71);
						      if (tree_swap_operands_p (_q70, _q71))
						        std::swap (_q70, _q71);
						      if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
							{
							  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
							    {
							      if (integer_onep (_q61))
								{
								  {
								    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
								    if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
								      return true;
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
								    if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
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
				    {
				      tree _q60_pops[1];
				      if (gimple_nop_convert (_q60, _q60_pops, valueize))
				        {
					  tree _q70 = _q60_pops[0];
					  switch (TREE_CODE (_q70))
					    {
					    case SSA_NAME:
					      if (gimple *_d3 = get_def (valueize, _q70))
					        {
						  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						    switch (gimple_assign_rhs_code (_a3))
						      {
						      case BIT_IOR_EXPR:
						        {
							  tree _q80 = gimple_assign_rhs1 (_a3);
							  _q80 = do_valueize (valueize, _q80);
							  tree _q81 = gimple_assign_rhs2 (_a3);
							  _q81 = do_valueize (valueize, _q81);
							  if (tree_swap_operands_p (_q80, _q81))
							    std::swap (_q80, _q81);
							  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
							    {
							      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
								{
								  if (integer_onep (_q61))
								    {
								      {
									tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
									if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
									  return true;
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
									if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
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
			  tree _q60 = _p1_pops[0];
			  switch (TREE_CODE (_q60))
			    {
			    case SSA_NAME:
			      if (gimple *_d2 = get_def (valueize, _q60))
			        {
				  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
				    switch (gimple_assign_rhs_code (_a2))
				      {
				      case PLUS_EXPR:
				        {
					  tree _q70 = gimple_assign_rhs1 (_a2);
					  _q70 = do_valueize (valueize, _q70);
					  tree _q71 = gimple_assign_rhs2 (_a2);
					  _q71 = do_valueize (valueize, _q71);
					  if (tree_swap_operands_p (_q70, _q71))
					    std::swap (_q70, _q71);
					  switch (TREE_CODE (_q70))
					    {
					    case SSA_NAME:
					      if (gimple *_d3 = get_def (valueize, _q70))
					        {
						  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						    switch (gimple_assign_rhs_code (_a3))
						      {
						      case BIT_IOR_EXPR:
						        {
							  tree _q80 = gimple_assign_rhs1 (_a3);
							  _q80 = do_valueize (valueize, _q80);
							  tree _q81 = gimple_assign_rhs2 (_a3);
							  _q81 = do_valueize (valueize, _q81);
							  if (tree_swap_operands_p (_q80, _q81))
							    std::swap (_q80, _q81);
							  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
							    {
							      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
								{
								  if (integer_onep (_q71))
								    {
								      {
									tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q60 };
									if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
									  return true;
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
									if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
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
					{
					  tree _q70_pops[1];
					  if (gimple_nop_convert (_q70, _q70_pops, valueize))
					    {
					      tree _q80 = _q70_pops[0];
					      switch (TREE_CODE (_q80))
					        {
						case SSA_NAME:
						  if (gimple *_d3 = get_def (valueize, _q80))
						    {
						      if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						        switch (gimple_assign_rhs_code (_a3))
							  {
							  case BIT_IOR_EXPR:
							    {
							      tree _q90 = gimple_assign_rhs1 (_a3);
							      _q90 = do_valueize (valueize, _q90);
							      tree _q91 = gimple_assign_rhs2 (_a3);
							      _q91 = do_valueize (valueize, _q91);
							      if (tree_swap_operands_p (_q90, _q91))
							        std::swap (_q90, _q91);
							      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
								{
								  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
								    {
								      if (integer_onep (_q71))
									{
									  {
									    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q60 };
									    if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
									      return true;
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
									    if (gimple_simplify_87 (res_op, seq, valueize, type, captures))
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
  if (gimple_negate_expr_p (_p1, valueize))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (!FIXED_POINT_TYPE_P (type)
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2071;
	    {
	      res_op->set_op (PLUS_EXPR, type, 2);
	      res_op->ops[0] = captures[0];
	      {
		tree _o1[1], _r1;
		_o1[0] = captures[1];
		gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		tem_op.resimplify (lseq, valueize);
		_r1 = maybe_push_res_to_seq (&tem_op, lseq);
		if (!_r1) goto next_after_fail2071;
		res_op->ops[1] = _r1;
	      }
	      res_op->resimplify (lseq, valueize);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1021, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2071:;
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
						  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
						    {
						      {
							tree captures[5] ATTRIBUTE_UNUSED = { _q50, _p1, _q40, _q51, _q20 };
							if (gimple_simplify_90 (res_op, seq, valueize, type, captures))
							  return true;
						      }
						    }
						  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
						    {
						      {
							tree captures[5] ATTRIBUTE_UNUSED = { _q51, _p1, _q40, _q50, _q20 };
							if (gimple_simplify_90 (res_op, seq, valueize, type, captures))
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
			      case BIT_AND_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _q20, 0))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _p1, _p1, _q41, _q20 };
					if (gimple_simplify_91 (res_op, seq, valueize, type, captures))
					  return true;
				      }
				    }
				  if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q20, 0))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _p1, _p1, _q40, _q20 };
					if (gimple_simplify_91 (res_op, seq, valueize, type, captures))
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
							      case POINTER_PLUS_EXPR:
							        {
								  tree _q50 = gimple_assign_rhs1 (_a4);
								  _q50 = do_valueize (valueize, _q50);
								  tree _q51 = gimple_assign_rhs2 (_a4);
								  _q51 = do_valueize (valueize, _q51);
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
										gimple_seq *lseq = seq;
										if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2072;
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
											if (!_r1) goto next_after_fail2072;
										      }
										    else
										      _r1 = _o1[0];
										    res_op->ops[1] = _r1;
										  }
										  res_op->resimplify (lseq, valueize);
										  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1022, __FILE__, __LINE__, true);
										  return true;
										}
next_after_fail2072:;
									      }
									}
								      }
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
						    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
						    if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
						      {
							{
 poly_int64 diff;
							    if (ptr_difference_const (captures[0], captures[1], &diff)
)
							      {
								gimple_seq *lseq = seq;
								if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2073;
								{
								  tree tem;
								  tem =  build_int_cst_type (type, diff);
								  res_op->set_value (tem);
								  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1023, __FILE__, __LINE__, true);
								  return true;
								}
next_after_fail2073:;
							      }
							}
						      }
						  }
					          break;
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
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
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
										gimple_seq *lseq = seq;
										if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2074;
										{
										  res_op->set_op (PLUS_EXPR, type, 2);
										  {
										    tree _o1[1], _r1;
										    _o1[0] = captures[1];
										    if (type != TREE_TYPE (_o1[0])
										        && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
										      {
											gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
											tem_op.resimplify (lseq, valueize);
											_r1 = maybe_push_res_to_seq (&tem_op, lseq);
											if (!_r1) goto next_after_fail2074;
										      }
										    else
										      _r1 = _o1[0];
										    res_op->ops[0] = _r1;
										  }
										  res_op->ops[1] =  build_int_cst_type (type, diff);
										  res_op->resimplify (lseq, valueize);
										  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1024, __FILE__, __LINE__, true);
										  return true;
										}
next_after_fail2074:;
									      }
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
							  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q60 };
							  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
							    {
							      {
 poly_int64 diff;
								  if (ptr_difference_const (captures[0], captures[2], &diff)
)
								    {
								      gimple_seq *lseq = seq;
								      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2075;
								      {
									res_op->set_op (PLUS_EXPR, type, 2);
									{
									  tree _o1[1], _r1;
									  _o1[0] = captures[1];
									  if (type != TREE_TYPE (_o1[0])
									      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
									    {
									      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
									      tem_op.resimplify (lseq, valueize);
									      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
									      if (!_r1) goto next_after_fail2075;
									    }
									  else
									    _r1 = _o1[0];
									  res_op->ops[0] = _r1;
									}
									res_op->ops[1] =  build_int_cst_type (type, diff);
									res_op->resimplify (lseq, valueize);
									if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1024, __FILE__, __LINE__, true);
									return true;
								      }
next_after_fail2075:;
								    }
							      }
							    }
							}
						        break;
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
						    case POINTER_PLUS_EXPR:
						      {
							tree _q50 = gimple_assign_rhs1 (_a3);
							_q50 = do_valueize (valueize, _q50);
							tree _q51 = gimple_assign_rhs2 (_a3);
							_q51 = do_valueize (valueize, _q51);
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
								      gimple_seq *lseq = seq;
								      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2076;
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
									      if (!_r1) goto next_after_fail2076;
									    }
									  else
									    _r1 = _o1[0];
									  res_op->ops[1] = _r1;
									}
									res_op->resimplify (lseq, valueize);
									if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1022, __FILE__, __LINE__, true);
									return true;
								      }
next_after_fail2076:;
								    }
							      }
							    }
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
					  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
					  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
					    {
					      {
 poly_int64 diff;
						  if (ptr_difference_const (captures[0], captures[1], &diff)
)
						    {
						      gimple_seq *lseq = seq;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2077;
						      {
							tree tem;
							tem =  build_int_cst_type (type, diff);
							res_op->set_value (tem);
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1023, __FILE__, __LINE__, true);
							return true;
						      }
next_after_fail2077:;
						    }
					      }
					    }
					}
				        break;
				      }
				    default:;
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
						    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
						    if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
						      {
							{
 poly_int64 diff;
							    if (ptr_difference_const (captures[0], captures[1], &diff)
)
							      {
								gimple_seq *lseq = seq;
								if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2078;
								{
								  tree tem;
								  tem =  build_int_cst_type (type, diff);
								  res_op->set_value (tem);
								  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1025, __FILE__, __LINE__, true);
								  return true;
								}
next_after_fail2078:;
							      }
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
					  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
					  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
					    {
					      {
 poly_int64 diff;
						  if (ptr_difference_const (captures[0], captures[1], &diff)
)
						    {
						      gimple_seq *lseq = seq;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2079;
						      {
							tree tem;
							tem =  build_int_cst_type (type, diff);
							res_op->set_value (tem);
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1025, __FILE__, __LINE__, true);
							return true;
						      }
next_after_fail2079:;
						    }
					      }
					    }
					}
				        break;
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
		gimple_seq *lseq = seq;
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2080;
		{
		  res_op->set_op (BIT_XOR_EXPR, type, 2);
		  res_op->ops[0] = captures[1];
		  res_op->ops[1] = captures[0];
		  res_op->resimplify (lseq, valueize);
		  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1026, __FILE__, __LINE__, true);
		  return true;
		}
next_after_fail2080:;
	      }
	  }
          break;
        }
      default:;
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
			      case BIT_AND_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if (tree_swap_operands_p (_q40, _q41))
				    std::swap (_q40, _q41);
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _p1, _q30, _q41, _p0 };
					if (gimple_simplify_90 (res_op, seq, valueize, type, captures))
					  return true;
				      }
				    }
				  if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _p1, _q30, _q40, _p0 };
					if (gimple_simplify_90 (res_op, seq, valueize, type, captures))
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
	      case BIT_AND_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if (tree_swap_operands_p (_q30, _q31))
		    std::swap (_q30, _q31);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || operand_equal_p (_q30, _p0, 0))
		    {
		      {
			tree captures[5] ATTRIBUTE_UNUSED = { _q30, _p1, _p1, _q31, _p0 };
			if (gimple_simplify_91 (res_op, seq, valueize, type, captures))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _p0, 0))
		    {
		      {
			tree captures[5] ATTRIBUTE_UNUSED = { _q31, _p1, _p1, _q30, _p0 };
			if (gimple_simplify_91 (res_op, seq, valueize, type, captures))
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2081;
					    {
					      res_op->set_op (MINUS_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2081:;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2082;
					    {
					      res_op->set_op (MINUS_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2082:;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2083;
					    {
					      res_op->set_op (MINUS_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2083:;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2084;
					    {
					      res_op->set_op (MINUS_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2084:;
					  }
				      }
				    }
			          break;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2085;
					    {
					      res_op->set_op (MINUS_EXPR, type, 2);
					      res_op->ops[0] = captures[0];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1028, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2085:;
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2086;
					    {
					      res_op->set_op (MINUS_EXPR, type, 2);
					      res_op->ops[0] = captures[2];
					      res_op->ops[1] = captures[1];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1029, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2086:;
					  }
				      }
				    }
			          break;
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
					if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[2]))
)
					  {
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2087;
					    {
					      res_op->set_op (POINTER_DIFF_EXPR, type, 2);
					      res_op->ops[0] = captures[1];
					      res_op->ops[1] = captures[3];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1030, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2087:;
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
					    gimple_seq *lseq = seq;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2088;
					    {
					      res_op->set_op (POINTER_DIFF_EXPR, type, 2);
					      res_op->ops[0] = captures[3];
					      res_op->ops[1] = captures[2];
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1031, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2088:;
					  }
				      }
				    }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
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
				    if (gimple_simplify_92 (res_op, seq, valueize, type, captures))
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
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		    if (gimple_simplify_92 (res_op, seq, valueize, type, captures))
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
	      case PLUS_EXPR:
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
			if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
			      if (gimple_simplify_93 (res_op, seq, valueize, type, captures))
			        return true;
			    }
			  }
		      }
		  }
		  {
		    tree _q21_pops[1];
		    if (gimple_nop_convert (_q21, _q21_pops, valueize))
		      {
			tree _q40 = _q21_pops[0];
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
			      if (gimple_simplify_93 (res_op, seq, valueize, type, captures))
			        return true;
			    }
			  }
		      }
		  }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_93 (res_op, seq, valueize, type, captures))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_93 (res_op, seq, valueize, type, captures))
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
		  {
		    tree _q20_pops[1];
		    if (gimple_nop_convert (_q20, _q20_pops, valueize))
		      {
			tree _q30 = _q20_pops[0];
			if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
			      if (gimple_simplify_94 (res_op, seq, valueize, type, captures))
			        return true;
			    }
			  }
		      }
		  }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_94 (res_op, seq, valueize, type, captures))
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
		  case PLUS_EXPR:
		    {
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      tree _q31 = gimple_assign_rhs2 (_a1);
		      _q31 = do_valueize (valueize, _q31);
		      if (tree_swap_operands_p (_q30, _q31))
		        std::swap (_q30, _q31);
		      {
			tree _q30_pops[1];
			if (gimple_nop_convert (_q30, _q30_pops, valueize))
			  {
			    tree _q40 = _q30_pops[0];
			    if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q31 };
				  if (gimple_simplify_93 (res_op, seq, valueize, type, captures))
				    return true;
				}
			      }
			  }
		      }
		      {
			tree _q31_pops[1];
			if (gimple_nop_convert (_q31, _q31_pops, valueize))
			  {
			    tree _q50 = _q31_pops[0];
			    if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q30 };
				  if (gimple_simplify_93 (res_op, seq, valueize, type, captures))
				    return true;
				}
			      }
			  }
		      }
		      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			    if (gimple_simplify_93 (res_op, seq, valueize, type, captures))
			      return true;
			  }
		        }
		      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
			    if (gimple_simplify_93 (res_op, seq, valueize, type, captures))
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
		      {
			tree _q30_pops[1];
			if (gimple_nop_convert (_q30, _q30_pops, valueize))
			  {
			    tree _q40 = _q30_pops[0];
			    if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q31 };
				  if (gimple_simplify_94 (res_op, seq, valueize, type, captures))
				    return true;
				}
			      }
			  }
		      }
		      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			    if (gimple_simplify_94 (res_op, seq, valueize, type, captures))
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
		  {
		    tree _q30_pops[1];
		    if (gimple_nop_convert (_q30, _q30_pops, valueize))
		      {
			tree _q40 = _q30_pops[0];
			if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			      if (gimple_simplify_95 (res_op, seq, valueize, type, captures))
			        return true;
			    }
			  }
		      }
		  }
		  {
		    tree _q31_pops[1];
		    if (gimple_nop_convert (_q31, _q31_pops, valueize))
		      {
			tree _q50 = _q31_pops[0];
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			      if (gimple_simplify_95 (res_op, seq, valueize, type, captures))
			        return true;
			    }
			  }
		      }
		  }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			if (gimple_simplify_95 (res_op, seq, valueize, type, captures))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_95 (res_op, seq, valueize, type, captures))
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
		  {
		    tree _q30_pops[1];
		    if (gimple_nop_convert (_q30, _q30_pops, valueize))
		      {
			tree _q40 = _q30_pops[0];
			if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			      if (gimple_simplify_96 (res_op, seq, valueize, type, captures))
			        return true;
			    }
			  }
		      }
		  }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			if (gimple_simplify_96 (res_op, seq, valueize, type, captures))
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
		      {
			tree _q40_pops[1];
			if (gimple_nop_convert (_q40, _q40_pops, valueize))
			  {
			    tree _q50 = _q40_pops[0];
			    if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
				  if (gimple_simplify_95 (res_op, seq, valueize, type, captures))
				    return true;
				}
			      }
			  }
		      }
		      {
			tree _q41_pops[1];
			if (gimple_nop_convert (_q41, _q41_pops, valueize))
			  {
			    tree _q60 = _q41_pops[0];
			    if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
				  if (gimple_simplify_95 (res_op, seq, valueize, type, captures))
				    return true;
				}
			      }
			  }
		      }
		      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
			    if (gimple_simplify_95 (res_op, seq, valueize, type, captures))
			      return true;
			  }
		        }
		      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
			    if (gimple_simplify_95 (res_op, seq, valueize, type, captures))
			      return true;
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
			tree _q40_pops[1];
			if (gimple_nop_convert (_q40, _q40_pops, valueize))
			  {
			    tree _q50 = _q40_pops[0];
			    if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
				  if (gimple_simplify_96 (res_op, seq, valueize, type, captures))
				    return true;
				}
			      }
			  }
		      }
		      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
			    if (gimple_simplify_96 (res_op, seq, valueize, type, captures))
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
			      case MINUS_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_97 (res_op, seq, valueize, type, captures))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_97 (res_op, seq, valueize, type, captures))
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
		  if (CONSTANT_CLASS_P (_q21))
		    {
		      if (CONSTANT_CLASS_P (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_60 (res_op, seq, valueize, type, captures, PLUS_EXPR, MINUS_EXPR, MINUS_EXPR))
			      return true;
			  }
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
		  if (CONSTANT_CLASS_P (_q21))
		    {
		      if (CONSTANT_CLASS_P (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_60 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR, MINUS_EXPR))
			      return true;
			  }
		        }
		    }
		  if (CONSTANT_CLASS_P (_q20))
		    {
		      if (CONSTANT_CLASS_P (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_61 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
				    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
				    if (gimple_simplify_66 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  if (integer_each_onep (_p1))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					if (gimple_simplify_99 (res_op, seq, valueize, type, captures))
					  return true;
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
						  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q30, 0))
						    {
						      {
							tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q30 };
							if (gimple_simplify_100 (res_op, seq, valueize, type, captures))
							  return true;
						      }
						    }
						  if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q31, 0))
						    {
						      {
							tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q31 };
							if (gimple_simplify_100 (res_op, seq, valueize, type, captures))
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
				  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q30, 0))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q30 };
					if (gimple_simplify_100 (res_op, seq, valueize, type, captures))
					  return true;
				      }
				    }
				  if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q31, 0))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q31 };
					if (gimple_simplify_100 (res_op, seq, valueize, type, captures))
					  return true;
				      }
				    }
			          break;
			        }
			      case POINTER_PLUS_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
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
								    gimple_seq *lseq = seq;
								    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2089;
								    {
								      res_op->set_op (NOP_EXPR, type, 1);
								      res_op->ops[0] = captures[1];
								      res_op->resimplify (lseq, valueize);
								      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1032, __FILE__, __LINE__, true);
								      return true;
								    }
next_after_fail2089:;
								  }
							      }
							  }
						      }
						    }
					          break;
					        }
					      default:;
					      }
				        }
				      break;
				    default:;
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
					      case PLUS_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
						  if (tree_swap_operands_p (_q50, _q51))
						    std::swap (_q50, _q51);
						  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
						    {
						      {
							tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q20 };
							if (gimple_simplify_101 (res_op, seq, valueize, type, captures))
							  return true;
						      }
						    }
						  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
						    {
						      {
							tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q20 };
							if (gimple_simplify_101 (res_op, seq, valueize, type, captures))
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
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  if (integer_each_onep (_p1))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			if (gimple_simplify_99 (res_op, seq, valueize, type, captures))
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
		  case PLUS_EXPR:
		    {
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      tree _q31 = gimple_assign_rhs2 (_a1);
		      _q31 = do_valueize (valueize, _q31);
		      if (tree_swap_operands_p (_q30, _q31))
		        std::swap (_q30, _q31);
		      if (CONSTANT_CLASS_P (_q31))
			{
			  if (CONSTANT_CLASS_P (_p1))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
				if (gimple_simplify_60 (res_op, seq, valueize, type, captures, PLUS_EXPR, MINUS_EXPR, MINUS_EXPR))
				  return true;
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
		      if (CONSTANT_CLASS_P (_q31))
			{
			  if (CONSTANT_CLASS_P (_p1))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
				if (gimple_simplify_60 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR, MINUS_EXPR))
				  return true;
			      }
			    }
		        }
		      if (CONSTANT_CLASS_P (_q30))
			{
			  if (CONSTANT_CLASS_P (_p1))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
				if (gimple_simplify_61 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
    }
}
if (CONSTANT_CLASS_P (_p0))
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
		    if (CONSTANT_CLASS_P (_q30))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
			  if (gimple_simplify_98 (res_op, seq, valueize, type, captures))
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
		    case MINUS_EXPR:
		      {
			tree _q40 = gimple_assign_rhs1 (_a1);
			_q40 = do_valueize (valueize, _q40);
			tree _q41 = gimple_assign_rhs2 (_a1);
			_q41 = do_valueize (valueize, _q41);
			if (CONSTANT_CLASS_P (_q40))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q41 };
			      if (gimple_simplify_98 (res_op, seq, valueize, type, captures))
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
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2090;
		  {
		    res_op->set_op (BIT_NOT_EXPR, type, 1);
		    res_op->ops[0] = captures[0];
		    res_op->resimplify (lseq, valueize);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1033, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail2090:;
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
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _p0 };
					if (gimple_simplify_101 (res_op, seq, valueize, type, captures))
					  return true;
				      }
				    }
				  if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _p0 };
					if (gimple_simplify_101 (res_op, seq, valueize, type, captures))
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
					      case POINTER_PLUS_EXPR:
					        {
						  tree _q50 = gimple_assign_rhs1 (_a3);
						  _q50 = do_valueize (valueize, _q50);
						  tree _q51 = gimple_assign_rhs2 (_a3);
						  _q51 = do_valueize (valueize, _q51);
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
									gimple_seq *lseq = seq;
									if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2091;
									{
									  res_op->set_op (NOP_EXPR, type, 1);
									  {
									    tree _o1[1], _r1;
									    {
									      tree _o2[1], _r2;
									      _o2[0] = captures[1];
									      if (utype != TREE_TYPE (_o2[0])
									          && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
										{
										  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
										  tem_op.resimplify (lseq, valueize);
										  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
										  if (!_r2) goto next_after_fail2091;
									        }
									      else
									        _r2 = _o2[0];
									      _o1[0] = _r2;
									    }
									    gimple_match_op tem_op (res_op->cond.any_else (), NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
									    tem_op.resimplify (lseq, valueize);
									    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
									    if (!_r1) goto next_after_fail2091;
									    res_op->ops[0] = _r1;
									  }
									  res_op->resimplify (lseq, valueize);
									  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1034, __FILE__, __LINE__, true);
									  return true;
									}
next_after_fail2091:;
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
									gimple_seq *lseq = seq;
									if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2092;
									{
									  res_op->set_op (NEGATE_EXPR, type, 1);
									  {
									    tree _o1[1], _r1;
									    _o1[0] = captures[1];
									    if (type != TREE_TYPE (_o1[0])
									        && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
									      {
										gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
										tem_op.resimplify (lseq, valueize);
										_r1 = maybe_push_res_to_seq (&tem_op, lseq);
										if (!_r1) goto next_after_fail2092;
									      }
									    else
									      _r1 = _o1[0];
									    res_op->ops[0] = _r1;
									  }
									  res_op->resimplify (lseq, valueize);
									  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1035, __FILE__, __LINE__, true);
									  return true;
									}
next_after_fail2092:;
								      }
								  }
							      }
							  }
						      }
						    }
					          break;
					        }
					      default:;
					      }
				        }
				      break;
				    default:;
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
			      case PLUS_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (tree_swap_operands_p (_q30, _q31))
				    std::swap (_q30, _q31);
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
							      case PLUS_EXPR:
							        {
								  tree _q70 = gimple_assign_rhs1 (_a4);
								  _q70 = do_valueize (valueize, _q70);
								  tree _q71 = gimple_assign_rhs2 (_a4);
								  _q71 = do_valueize (valueize, _q71);
								  if (tree_swap_operands_p (_q70, _q71))
								    std::swap (_q70, _q71);
								  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q30, 0))
								    {
								      {
									tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q71, _q30 };
									if (gimple_simplify_102 (res_op, seq, valueize, type, captures))
									  return true;
								      }
								    }
								  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q30, 0))
								    {
								      {
									tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q70, _q30 };
									if (gimple_simplify_102 (res_op, seq, valueize, type, captures))
									  return true;
								      }
								    }
								  if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q31, 0))
								    {
								      {
									tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q71, _q31 };
									if (gimple_simplify_102 (res_op, seq, valueize, type, captures))
									  return true;
								      }
								    }
								  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q31, 0))
								    {
								      {
									tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q70, _q31 };
									if (gimple_simplify_102 (res_op, seq, valueize, type, captures))
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
			      case POINTER_PLUS_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
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
							      case POINTER_PLUS_EXPR:
							        {
								  tree _q70 = gimple_assign_rhs1 (_a4);
								  _q70 = do_valueize (valueize, _q70);
								  tree _q71 = gimple_assign_rhs2 (_a4);
								  _q71 = do_valueize (valueize, _q71);
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
											gimple_seq *lseq = seq;
											if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2093;
											{
											  res_op->set_op (NOP_EXPR, type, 1);
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
												  if (!_r2) goto next_after_fail2093;
											        }
											      else
											        _r2 = _o2[0];
											      _o1[0] = _r2;
											    }
											    {
											      tree _o2[1], _r2;
											      _o2[0] = captures[2];
											      if (utype != TREE_TYPE (_o2[0])
											          && !useless_type_conversion_p (utype, TREE_TYPE (_o2[0])))
												{
												  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o2[0]);
												  tem_op.resimplify (lseq, valueize);
												  _r2 = maybe_push_res_to_seq (&tem_op, lseq);
												  if (!_r2) goto next_after_fail2093;
											        }
											      else
											        _r2 = _o2[0];
											      _o1[1] = _r2;
											    }
											    gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
											    tem_op.resimplify (lseq, valueize);
											    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
											    if (!_r1) goto next_after_fail2093;
											    res_op->ops[0] = _r1;
											  }
											  res_op->resimplify (lseq, valueize);
											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1036, __FILE__, __LINE__, true);
											  return true;
											}
next_after_fail2093:;
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
											gimple_seq *lseq = seq;
											if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2094;
											{
											  res_op->set_op (MINUS_EXPR, type, 2);
											  {
											    tree _o1[1], _r1;
											    _o1[0] = captures[1];
											    if (type != TREE_TYPE (_o1[0])
											        && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
											      {
												gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
												tem_op.resimplify (lseq, valueize);
												_r1 = maybe_push_res_to_seq (&tem_op, lseq);
												if (!_r1) goto next_after_fail2094;
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
												if (!_r1) goto next_after_fail2094;
											      }
											    else
											      _r1 = _o1[0];
											    res_op->ops[1] = _r1;
											  }
											  res_op->resimplify (lseq, valueize);
											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1037, __FILE__, __LINE__, true);
											  return true;
											}
next_after_fail2094:;
										      }
										  }
									      }
									  }
								      }
								    }
							          break;
							        }
							      default:;
							      }
						        }
						      break;
						    default:;
						    }
					          break;
					        }
					      default:;
					      }
				        }
				      break;
				    default:;
				    }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
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
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
					if (gimple_simplify_69 (res_op, seq, valueize, type, captures, MINUS_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
					if (gimple_simplify_69 (res_op, seq, valueize, type, captures, MINUS_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
					if (gimple_simplify_69 (res_op, seq, valueize, type, captures, MINUS_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
					if (gimple_simplify_69 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
	      case MULT_EXPR:
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
			if (gimple_simplify_70 (res_op, seq, valueize, type, captures, MINUS_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
			if (gimple_simplify_70 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_71 (res_op, seq, valueize, type, captures, MINUS_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			if (gimple_simplify_71 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
			      case MINUS_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
				    {
				      if (gimple_zero_one_valued_p (_q31, valueize))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q41, _q31 };
					    if (gimple_simplify_103 (res_op, seq, valueize, type, captures))
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
		if (gimple_zero_one_valued_p (_q30, valueize))
		  {
		    switch (TREE_CODE (_q31))
		      {
		      case SSA_NAME:
		        if (gimple *_d2 = get_def (valueize, _q31))
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
				    if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q51, _q30 };
					  if (gimple_simplify_103 (res_op, seq, valueize, type, captures))
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
 && (TREE_CODE_CLASS (MINUS_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2095;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2095;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2095;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2095:;
					  }
				      }
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
 && (TREE_CODE_CLASS (MINUS_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2096;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2096;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2096;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2096:;
		      }
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2097;
      {
	res_op->set_op (NEGATE_EXPR, type, 1);
	res_op->ops[0] = captures[0];
	res_op->resimplify (lseq, valueize);
	if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1038, __FILE__, __LINE__, true);
	return true;
      }
next_after_fail2097:;
    }
  }
if (real_zerop (_p0))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (fold_real_zero_addition_p (type, captures[1], captures[0], 0)
)
	{
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2098;
	  {
	    res_op->set_op (NEGATE_EXPR, type, 1);
	    res_op->ops[0] = captures[1];
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1039, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail2098:;
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
 && (TREE_CODE_CLASS (MINUS_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2099;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2099;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2099;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2099:;
		      }
		  }
		  if (integer_each_onep (_q31))
		    {
		      if (integer_zerop (_q32))
			{
			  {
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32 };
			    if (gimple_simplify_104 (res_op, seq, valueize, type, captures))
			      return true;
			  }
		        }
		    }
	          break;
	        }
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
				  case VEC_COND_EXPR:
				    {
				      tree _q40 = gimple_assign_rhs1 (_a2);
				      _q40 = do_valueize (valueize, _q40);
				      tree _q41 = gimple_assign_rhs2 (_a2);
				      _q41 = do_valueize (valueize, _q41);
				      tree _q42 = gimple_assign_rhs3 (_a2);
				      _q42 = do_valueize (valueize, _q42);
				      if (integer_each_onep (_q41))
					{
					  if (integer_zerop (_q42))
					    {
					      {
						tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _q42 };
						if (gimple_simplify_104 (res_op, seq, valueize, type, captures))
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
	      case RDIV_EXPR:
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
			      case RDIV_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					if (gimple_simplify_76 (res_op, seq, valueize, type, captures, MINUS_EXPR))
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
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gcall *_c2 = dyn_cast <gcall *> (_d2))
			    switch (gimple_call_combined_fn (_c2))
			      {
			      case CFN_BUILT_IN_POPCOUNT:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gcall *_c3 = dyn_cast <gcall *> (_d3))
					      switch (gimple_call_combined_fn (_c3))
						{
						case CFN_BUILT_IN_POPCOUNT:
						  if (gimple_call_num_args (_c3) == 1)
						    {
						      tree _q50 = gimple_call_arg (_c3, 0);
						      _q50 = do_valueize (valueize, _q50);
						      switch (TREE_CODE (_p1))
						        {
							case SSA_NAME:
							  if (gimple *_d4 = get_def (valueize, _p1))
							    {
							      if (gcall *_c4 = dyn_cast <gcall *> (_d4))
							        switch (gimple_call_combined_fn (_c4))
								  {
								  case CFN_BUILT_IN_POPCOUNT:
								    if (gimple_call_num_args (_c4) == 1)
								      {
									tree _q70 = gimple_call_arg (_c4, 0);
									_q70 = do_valueize (valueize, _q70);
									switch (TREE_CODE (_q70))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q70))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case BIT_AND_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNT))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNT))
												    return true;
												}
											      }
											  }
										        break;
										      }
										    case BIT_IOR_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNT))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNT))
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
			          }
			        break;
			      case CFN_BUILT_IN_POPCOUNTLL:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gcall *_c3 = dyn_cast <gcall *> (_d3))
					      switch (gimple_call_combined_fn (_c3))
						{
						case CFN_BUILT_IN_POPCOUNTLL:
						  if (gimple_call_num_args (_c3) == 1)
						    {
						      tree _q50 = gimple_call_arg (_c3, 0);
						      _q50 = do_valueize (valueize, _q50);
						      switch (TREE_CODE (_p1))
						        {
							case SSA_NAME:
							  if (gimple *_d4 = get_def (valueize, _p1))
							    {
							      if (gcall *_c4 = dyn_cast <gcall *> (_d4))
							        switch (gimple_call_combined_fn (_c4))
								  {
								  case CFN_BUILT_IN_POPCOUNTLL:
								    if (gimple_call_num_args (_c4) == 1)
								      {
									tree _q70 = gimple_call_arg (_c4, 0);
									_q70 = do_valueize (valueize, _q70);
									switch (TREE_CODE (_q70))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q70))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case BIT_AND_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTLL))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTLL))
												    return true;
												}
											      }
											  }
										        break;
										      }
										    case BIT_IOR_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTLL))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTLL))
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
			          }
			        break;
			      case CFN_POPCOUNT:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gcall *_c3 = dyn_cast <gcall *> (_d3))
					      switch (gimple_call_combined_fn (_c3))
						{
						case CFN_POPCOUNT:
						  if (gimple_call_num_args (_c3) == 1)
						    {
						      tree _q50 = gimple_call_arg (_c3, 0);
						      _q50 = do_valueize (valueize, _q50);
						      switch (TREE_CODE (_p1))
						        {
							case SSA_NAME:
							  if (gimple *_d4 = get_def (valueize, _p1))
							    {
							      if (gcall *_c4 = dyn_cast <gcall *> (_d4))
							        switch (gimple_call_combined_fn (_c4))
								  {
								  case CFN_POPCOUNT:
								    if (gimple_call_num_args (_c4) == 1)
								      {
									tree _q70 = gimple_call_arg (_c4, 0);
									_q70 = do_valueize (valueize, _q70);
									switch (TREE_CODE (_q70))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q70))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case BIT_AND_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_POPCOUNT))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_POPCOUNT))
												    return true;
												}
											      }
											  }
										        break;
										      }
										    case BIT_IOR_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_POPCOUNT))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_POPCOUNT))
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
			          }
			        break;
			      case CFN_BUILT_IN_POPCOUNTIMAX:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gcall *_c3 = dyn_cast <gcall *> (_d3))
					      switch (gimple_call_combined_fn (_c3))
						{
						case CFN_BUILT_IN_POPCOUNTIMAX:
						  if (gimple_call_num_args (_c3) == 1)
						    {
						      tree _q50 = gimple_call_arg (_c3, 0);
						      _q50 = do_valueize (valueize, _q50);
						      switch (TREE_CODE (_p1))
						        {
							case SSA_NAME:
							  if (gimple *_d4 = get_def (valueize, _p1))
							    {
							      if (gcall *_c4 = dyn_cast <gcall *> (_d4))
							        switch (gimple_call_combined_fn (_c4))
								  {
								  case CFN_BUILT_IN_POPCOUNTIMAX:
								    if (gimple_call_num_args (_c4) == 1)
								      {
									tree _q70 = gimple_call_arg (_c4, 0);
									_q70 = do_valueize (valueize, _q70);
									switch (TREE_CODE (_q70))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q70))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case BIT_AND_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTIMAX))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTIMAX))
												    return true;
												}
											      }
											  }
										        break;
										      }
										    case BIT_IOR_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTIMAX))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTIMAX))
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
			          }
			        break;
			      case CFN_BUILT_IN_POPCOUNTL:
			        if (gimple_call_num_args (_c2) == 1)
			          {
				    tree _q30 = gimple_call_arg (_c2, 0);
				    _q30 = do_valueize (valueize, _q30);
				    switch (TREE_CODE (_q21))
				      {
				      case SSA_NAME:
				        if (gimple *_d3 = get_def (valueize, _q21))
				          {
					    if (gcall *_c3 = dyn_cast <gcall *> (_d3))
					      switch (gimple_call_combined_fn (_c3))
						{
						case CFN_BUILT_IN_POPCOUNTL:
						  if (gimple_call_num_args (_c3) == 1)
						    {
						      tree _q50 = gimple_call_arg (_c3, 0);
						      _q50 = do_valueize (valueize, _q50);
						      switch (TREE_CODE (_p1))
						        {
							case SSA_NAME:
							  if (gimple *_d4 = get_def (valueize, _p1))
							    {
							      if (gcall *_c4 = dyn_cast <gcall *> (_d4))
							        switch (gimple_call_combined_fn (_c4))
								  {
								  case CFN_BUILT_IN_POPCOUNTL:
								    if (gimple_call_num_args (_c4) == 1)
								      {
									tree _q70 = gimple_call_arg (_c4, 0);
									_q70 = do_valueize (valueize, _q70);
									switch (TREE_CODE (_q70))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q70))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case BIT_AND_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTL))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTL))
												    return true;
												}
											      }
											  }
										        break;
										      }
										    case BIT_IOR_EXPR:
										      {
											tree _q80 = gimple_assign_rhs1 (_a5);
											_q80 = do_valueize (valueize, _q80);
											tree _q81 = gimple_assign_rhs2 (_a5);
											_q81 = do_valueize (valueize, _q81);
											if (tree_swap_operands_p (_q80, _q81))
											  std::swap (_q80, _q81);
											if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
											  {
											    if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTL))
												    return true;
												}
											      }
											  }
											if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
											  {
											    if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
											      {
												{
												  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
												  if (gimple_simplify_106 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTL))
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
	      case VEC_PERM_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  tree _q22 = gimple_assign_rhs3 (_a1);
		  _q22 = do_valueize (valueize, _q22);
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
				  if (gimple_vec_same_elem_p (_q31, valueize))
				    {
				      if ((_q21 == _q20 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q20, 0) && types_match (_q21, _q20)))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q22, _p1 };
					    if (gimple_simplify_107 (res_op, seq, valueize, type, captures))
					      return true;
					  }
				        }
				    }
				  if (gimple_vec_same_elem_p (_q30, valueize))
				    {
				      if ((_q21 == _q20 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q20, 0) && types_match (_q21, _q20)))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _q22, _p1 };
					    if (gimple_simplify_107 (res_op, seq, valueize, type, captures))
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
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_LOG:
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
				case CFN_BUILT_IN_LOG:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG))
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
	      case CFN_BUILT_IN_LOG2:
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
				case CFN_BUILT_IN_LOG2:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG2))
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
	      case CFN_BUILT_IN_LOGF:
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
				case CFN_BUILT_IN_LOGF:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOGF))
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
	      case CFN_BUILT_IN_LOGL:
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
				case CFN_BUILT_IN_LOGL:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOGL))
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
	      case CFN_BUILT_IN_LOG10F:
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
				case CFN_BUILT_IN_LOG10F:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG10F))
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
	      case CFN_BUILT_IN_LOG10L:
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
				case CFN_BUILT_IN_LOG10L:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG10L))
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
	      case CFN_LOG:
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
				case CFN_LOG:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_LOG))
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
	      case CFN_LOG2:
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
				case CFN_LOG2:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_LOG2))
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
	      case CFN_LOG10:
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
				case CFN_LOG10:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_LOG10))
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
	      case CFN_BUILT_IN_LOG10:
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
				case CFN_BUILT_IN_LOG10:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG10))
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
	      case CFN_BUILT_IN_LOG2F:
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
				case CFN_BUILT_IN_LOG2F:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG2F))
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
	      case CFN_BUILT_IN_LOG2L:
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
				case CFN_BUILT_IN_LOG2L:
				  if (gimple_call_num_args (_c2) == 1)
				    {
				      tree _q40 = gimple_call_arg (_c2, 0);
				      _q40 = do_valueize (valueize, _q40);
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
					if (gimple_simplify_105 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_LOG2L))
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
		  switch (TREE_CODE (_q30))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q30))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case MAX_EXPR:
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
								gimple_seq *lseq = seq;
								if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2100;
								{
								  res_op->set_op (BIT_AND_EXPR, type, 2);
								  res_op->ops[0] = captures[0];
								  res_op->ops[1] = captures[1];
								  res_op->resimplify (lseq, valueize);
								  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1040, __FILE__, __LINE__, true);
								  return true;
								}
next_after_fail2100:;
							      }
							    else
							      {
								gimple_seq *lseq = seq;
								if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2101;
								{
								  res_op->set_op (COND_EXPR, type, 3);
								  {
								    tree _o1[2], _r1;
								    _o1[0] = captures[0];
								    _o1[1] = captures[1];
								    gimple_match_op tem_op (res_op->cond.any_else (), LE_EXPR, boolean_type_node, _o1[0], _o1[1]);
								    tem_op.resimplify (lseq, valueize);
								    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
								    if (!_r1) goto next_after_fail2101;
								    res_op->ops[0] = _r1;
								  }
								  res_op->ops[1] = captures[0];
								  {
								    tree _o1[2], _r1;
								    _o1[0] = captures[0];
								    _o1[1] = captures[1];
								    gimple_match_op tem_op (res_op->cond.any_else (), BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
								    tem_op.resimplify (lseq, valueize);
								    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
								    if (!_r1) goto next_after_fail2101;
								    res_op->ops[2] = _r1;
								  }
								  res_op->resimplify (lseq, valueize);
								  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1041, __FILE__, __LINE__, true);
								  return true;
								}
next_after_fail2101:;
							      }
							  }
						    }
						  }
					          break;
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
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2102;
						    {
						      res_op->set_op (VEC_PERM_EXPR, type, 3);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[2];
							gimple_match_op tem_op (res_op->cond.any_else (), MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2102;
							captures[3] = _r1;
						      }
						      res_op->ops[0] = captures[3];
						      res_op->ops[1] = captures[3];
						      res_op->ops[2] = captures[1];
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1001, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2102:;
						  }
					      }
					    }
				        }
				      break;
				    }
				  default:;
				  }
			    }
			  break;
		        default:;
		        }
		      switch (TREE_CODE (_q22))
		        {
			case VECTOR_CST:
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
						    switch (TREE_CODE (_q62))
						      {
						      case VECTOR_CST:
						        {
							  {
							    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q22, _q60 };
							    if (gimple_simplify_82 (res_op, seq, valueize, type, captures, MINUS_EXPR))
							      return true;
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
  return false;
}

bool
gimple_simplify_LT_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
			      if (gimple_simplify_347 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
			        return true;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_348 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, LT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, LT_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, LT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, LT_EXPR))
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
									      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, LT_EXPR))
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
				if (gimple_simplify_220 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, LT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, LT_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, LT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					      if (gimple_simplify_351 (res_op, seq, valueize, type, captures, LT_EXPR))
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
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, LT_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					if (gimple_simplify_353 (res_op, seq, valueize, type, captures, LT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_354 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					if (gimple_simplify_224 (res_op, seq, valueize, type, captures, LT_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
					if (gimple_simplify_225 (res_op, seq, valueize, type, captures, LT_EXPR))
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
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, GT_EXPR))
			  return true;
		      }
		    }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, GT_EXPR))
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
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, LT_EXPR))
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
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, GT_EXPR))
			  return true;
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
		  if (tree_expr_nonnegative_p (_q20))
		    {
		      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, LT_EXPR, BIT_IOR_EXPR))
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
		      if (tree_expr_nonnegative_p (_q30))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, GT_EXPR, BIT_AND_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, LT_EXPR, GT_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, LT_EXPR, GT_EXPR))
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
				if (gimple_simplify_356 (res_op, seq, valueize, type, captures, MIN_EXPR, LT_EXPR, BIT_IOR_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR, LT_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR, LT_EXPR))
			  return true;
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
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MIN_EXPR, LT_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MIN_EXPR, LT_EXPR))
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
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
				if (gimple_simplify_356 (res_op, seq, valueize, type, captures, MAX_EXPR, LT_EXPR, BIT_AND_EXPR))
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
 && (TREE_CODE_CLASS (LT_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2437;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), LT_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2437;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), LT_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2437;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2437:;
					  }
				      }
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
 && (TREE_CODE_CLASS (LT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2438;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2438;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2438;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2438:;
		      }
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
 && (TREE_CODE_CLASS (LT_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2439;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), LT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2439;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LT_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2439;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2439:;
		      }
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
      if (gimple_simplify_358 (res_op, seq, valueize, type, captures, LT_EXPR, LE_EXPR))
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
			      if (gimple_simplify_253 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
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
	if (gimple_simplify_306 (res_op, seq, valueize, type, captures, LT_EXPR))
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
				    if (gimple_simplify_257 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					if (gimple_simplify_257 (res_op, seq, valueize, type, captures, LT_EXPR))
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
		      if (gimple_simplify_258 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
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
					if (gimple_simplify_259 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					    if (gimple_simplify_259 (res_op, seq, valueize, type, captures, LT_EXPR))
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
			  if (gimple_simplify_258 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
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
	  if (gimple_simplify_260 (res_op, seq, valueize, type, captures, LT_EXPR))
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, PLUS_EXPR, LT_EXPR))
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
			      if (gimple_simplify_359 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, MINUS_EXPR, LT_EXPR))
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
				if (gimple_simplify_262 (res_op, seq, valueize, type, captures, LT_EXPR))
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, LT_EXPR, LT_EXPR))
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
			  if (gimple_simplify_266 (res_op, seq, valueize, type, captures, LT_EXPR, LE_EXPR))
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
				if (gimple_simplify_360 (res_op, seq, valueize, type, captures, LT_EXPR))
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
						if (gimple_simplify_361 (res_op, seq, valueize, type, captures, LT_EXPR, GE_EXPR))
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
				    if (gimple_simplify_270 (res_op, seq, valueize, type, captures, LT_EXPR))
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
		    if (gimple_simplify_271 (res_op, seq, valueize, type, captures, LT_EXPR))
		      return true;
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, LT_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, LT_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, LT_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, LT_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, LT_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, LT_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_SQRT, LT_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_SQRT, LT_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, GT_EXPR))
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
								    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LT_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LT_EXPR))
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
						    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LT_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LT_EXPR))
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
						if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LT_EXPR))
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
				if (gimple_simplify_289 (res_op, seq, valueize, type, captures, LT_EXPR))
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
			if (gimple_simplify_362 (res_op, seq, valueize, type, captures, LT_EXPR))
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
						    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LT_EXPR))
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
				    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LT_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LT_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LT_EXPR))
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
				if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LT_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LT_EXPR))
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
		if (gimple_simplify_290 (res_op, seq, valueize, type, captures, LT_EXPR))
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
				      if (integer_zerop (_q41))
					{
					  {
					    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
					    if (TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
					      {
						gimple_seq *lseq = seq;
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2440;
						{
						  tree tem;
						  tem =  constant_boolean_node (false, type);
						  res_op->set_value (tem);
						  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1107, __FILE__, __LINE__, true);
						  return true;
						}
next_after_fail2440:;
					      }
					  }
				        }
				    }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
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
      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _p1 };
      if (gimple_simplify_363 (res_op, seq, valueize, type, captures, LT_EXPR))
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
							      if (gimple_simplify_364 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
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
							      if (gimple_simplify_364 (res_op, seq, valueize, type, captures, GT_EXPR, LE_EXPR))
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
			      if (gimple_simplify_359 (res_op, seq, valueize, type, captures, GT_EXPR, LE_EXPR))
			        return true;
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
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q31 };
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
				  if (integer_minus_onep (_q21))
				    {
				      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
					    if (gimple_simplify_366 (res_op, seq, valueize, type, captures, LT_EXPR))
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
				  case CFN_SUB_OVERFLOW:
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
					      if (gimple_simplify_367 (res_op, seq, valueize, type, captures, GT_EXPR, NE_EXPR))
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
				  case CFN_ADD_OVERFLOW:
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
					      if (gimple_simplify_368 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR))
					        return true;
					    }
					  }
					if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q31, _q30 };
					      if (gimple_simplify_368 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR))
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_all_onesp (_q20))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _p1 };
			if (gimple_simplify_369 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR))
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
		    case REAL_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case REAL_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_370 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
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
				if (gimple_simplify_371 (res_op, seq, valueize, type, captures, PLUS_EXPR, MINUS_EXPR, LT_EXPR, GT_EXPR))
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
				if (gimple_simplify_371 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR, LT_EXPR, GT_EXPR))
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
				if (gimple_simplify_296 (res_op, seq, valueize, type, captures, LT_EXPR, GT_EXPR))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, CFN_BUILT_IN_CTZ))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, CFN_BUILT_IN_CTZL))
			      return true;
			  }
		          break;
		        }
		      default:;
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, CFN_BUILT_IN_CTZIMAX))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, CFN_CTZ))
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
			    if (gimple_simplify_373 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, LT_EXPR, NE_EXPR, CFN_BUILT_IN_CTZLL))
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
gimple_simplify_GE_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
			      if (gimple_simplify_347 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
			        return true;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_348 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, GE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, GE_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, GE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_349 (res_op, seq, valueize, type, captures, GE_EXPR))
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
									      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					      if (gimple_simplify_217 (res_op, seq, valueize, type, captures, GE_EXPR))
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
				if (gimple_simplify_220 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, GE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, GE_EXPR))
					  return true;
				      }
				    }
				  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, GE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
					if (gimple_simplify_350 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					      if (gimple_simplify_351 (res_op, seq, valueize, type, captures, GE_EXPR))
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
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, GE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					if (gimple_simplify_353 (res_op, seq, valueize, type, captures, GE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
					if (gimple_simplify_354 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					if (gimple_simplify_224 (res_op, seq, valueize, type, captures, GE_EXPR))
					  return true;
				      }
				    }
				  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
					if (gimple_simplify_225 (res_op, seq, valueize, type, captures, GE_EXPR))
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
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, LE_EXPR))
			  return true;
		      }
		    }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
			if (gimple_simplify_352 (res_op, seq, valueize, type, captures, LE_EXPR))
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
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, GE_EXPR))
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
			if (gimple_simplify_223 (res_op, seq, valueize, type, captures, LE_EXPR))
			  return true;
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
		  if (tree_expr_nonnegative_p (_q20))
		    {
		      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, GE_EXPR, BIT_IOR_EXPR))
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
		      if (tree_expr_nonnegative_p (_q30))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			    if (gimple_simplify_355 (res_op, seq, valueize, type, captures, LE_EXPR, BIT_AND_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, GE_EXPR, LE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MIN_EXPR, GE_EXPR, LE_EXPR))
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
				if (gimple_simplify_356 (res_op, seq, valueize, type, captures, MIN_EXPR, GE_EXPR, BIT_AND_EXPR))
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
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, LE_EXPR, GE_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_233 (res_op, seq, valueize, type, captures, MAX_EXPR, LE_EXPR, GE_EXPR))
			  return true;
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
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MIN_EXPR, GE_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MIN_EXPR, GE_EXPR))
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
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MAX_EXPR, LE_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_357 (res_op, seq, valueize, type, captures, MAX_EXPR, LE_EXPR))
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
				if (gimple_simplify_356 (res_op, seq, valueize, type, captures, MAX_EXPR, GE_EXPR, BIT_IOR_EXPR))
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
 && (TREE_CODE_CLASS (GE_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2448;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), GE_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2448;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), GE_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2448;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2448:;
					  }
				      }
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
 && (TREE_CODE_CLASS (GE_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2449;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), GE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2449;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), GE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2449;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2449:;
		      }
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
 && (TREE_CODE_CLASS (GE_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2450;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), GE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2450;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), GE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2450;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2450:;
		      }
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
      if (gimple_simplify_358 (res_op, seq, valueize, type, captures, GE_EXPR, GT_EXPR))
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
			      if (gimple_simplify_253 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
	if (gimple_simplify_255 (res_op, seq, valueize, type, captures, GE_EXPR))
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
				    if (gimple_simplify_257 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					if (gimple_simplify_257 (res_op, seq, valueize, type, captures, GE_EXPR))
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
		      if (gimple_simplify_258 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
					if (gimple_simplify_259 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					    if (gimple_simplify_259 (res_op, seq, valueize, type, captures, GE_EXPR))
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
			  if (gimple_simplify_258 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
	  if (gimple_simplify_260 (res_op, seq, valueize, type, captures, GE_EXPR))
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, PLUS_EXPR, GE_EXPR))
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
			      if (gimple_simplify_359 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
				if (gimple_simplify_261 (res_op, seq, valueize, type, captures, MINUS_EXPR, GE_EXPR))
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
				if (gimple_simplify_262 (res_op, seq, valueize, type, captures, GE_EXPR))
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, GE_EXPR, GE_EXPR))
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
			  if (gimple_simplify_266 (res_op, seq, valueize, type, captures, GE_EXPR, GE_EXPR))
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
				if (gimple_simplify_360 (res_op, seq, valueize, type, captures, GE_EXPR))
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
						if (gimple_simplify_361 (res_op, seq, valueize, type, captures, GE_EXPR, LT_EXPR))
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
				    if (gimple_simplify_270 (res_op, seq, valueize, type, captures, GE_EXPR))
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
		    if (gimple_simplify_271 (res_op, seq, valueize, type, captures, GE_EXPR))
		      return true;
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, GE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTF, GE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, GE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRTL, GE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, GE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_SQRT, GE_EXPR))
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
					if (gimple_simplify_264 (res_op, seq, valueize, type, captures, CFN_SQRT, GE_EXPR))
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
			    if (gimple_simplify_263 (res_op, seq, valueize, type, captures, CFN_SQRT, GE_EXPR))
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
		    if (gimple_simplify_286 (res_op, seq, valueize, type, captures, LE_EXPR))
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
								    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GE_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GE_EXPR))
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
						    if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GE_EXPR))
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
							  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GE_EXPR))
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
						if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					  if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GE_EXPR))
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
				if (gimple_simplify_289 (res_op, seq, valueize, type, captures, GE_EXPR))
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
			if (gimple_simplify_362 (res_op, seq, valueize, type, captures, GE_EXPR))
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
						    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GE_EXPR))
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
				    if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GE_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GE_EXPR))
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
					  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GE_EXPR))
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
				if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GE_EXPR))
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
			  if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GE_EXPR))
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
		if (gimple_simplify_290 (res_op, seq, valueize, type, captures, GE_EXPR))
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
	if (gimple_simplify_363 (res_op, seq, valueize, type, captures, GE_EXPR))
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
							      if (gimple_simplify_364 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
							      if (gimple_simplify_364 (res_op, seq, valueize, type, captures, LE_EXPR, GT_EXPR))
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
			      if (gimple_simplify_359 (res_op, seq, valueize, type, captures, LE_EXPR, GT_EXPR))
			        return true;
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
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q31 };
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
				  if (integer_minus_onep (_q21))
				    {
				      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
					    if (gimple_simplify_366 (res_op, seq, valueize, type, captures, GE_EXPR))
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
				  case CFN_SUB_OVERFLOW:
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
					      if (gimple_simplify_367 (res_op, seq, valueize, type, captures, LE_EXPR, EQ_EXPR))
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
				  case CFN_ADD_OVERFLOW:
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
					      if (gimple_simplify_368 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR))
					        return true;
					    }
					  }
					if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q31, _q30 };
					      if (gimple_simplify_368 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR))
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (integer_all_onesp (_q20))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _p1 };
			if (gimple_simplify_369 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR))
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
		    case REAL_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case REAL_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (gimple_simplify_370 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
				if (gimple_simplify_371 (res_op, seq, valueize, type, captures, PLUS_EXPR, MINUS_EXPR, GE_EXPR, LE_EXPR))
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
				if (gimple_simplify_371 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR, GE_EXPR, LE_EXPR))
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
				if (gimple_simplify_296 (res_op, seq, valueize, type, captures, GE_EXPR, LE_EXPR))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZ))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZL))
			      return true;
			  }
		          break;
		        }
		      default:;
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZIMAX))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR, CFN_CTZ))
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
			    if (gimple_simplify_373 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR))
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
			    if (gimple_simplify_372 (res_op, seq, valueize, type, captures, GE_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZLL))
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
gimple_simplify_CFN_POW (gimple_match_op *res_op, gimple_seq *seq,
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
			  const combined_fn pows = CFN_POW;
			  {
 HOST_WIDE_INT n;
			      if (real_isinteger (&TREE_REAL_CST (captures[1]), &n) && (n & 1) == 0
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2523;
				  {
				    res_op->set_op (pows, type, 2);
				    res_op->ops[0] = captures[0];
				    res_op->ops[1] = captures[1];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1117, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail2523:;
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
			  const combined_fn pows = CFN_POW;
			  {
 HOST_WIDE_INT n;
			      if (real_isinteger (&TREE_REAL_CST (captures[1]), &n) && (n & 1) == 0
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2524;
				  {
				    res_op->set_op (pows, type, 2);
				    res_op->ops[0] = captures[0];
				    res_op->ops[1] = captures[1];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1117, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail2524:;
				}
			  }
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
	      case CFN_EXP:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2525;
			  {
			    res_op->set_op (CFN_EXP, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2525;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1119, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2525:;
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
		    if (tree_expr_nonnegative_p (_q20))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2526;
			      {
				res_op->set_op (CFN_POW, type, 2);
				res_op->ops[0] = captures[0];
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[1];
				  _o1[1] = captures[2];
				  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail2526;
				  res_op->ops[1] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1120, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2526:;
			    }
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
			    if (gimple_simplify_412 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_POW))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_EXP2:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2527;
			  {
			    res_op->set_op (CFN_EXP2, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2527;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1119, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2527:;
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
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2528;
			  {
			    res_op->set_op (CFN_POW, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[1];
			      _o1[1] =  build_real (type, dconsthalf);
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2528;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1118, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2528:;
			}
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
			    if (gimple_simplify_412 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_POW))
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
			    if (gimple_simplify_412 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_POW))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_EXP10:
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2529;
			  {
			    res_op->set_op (CFN_EXP10, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2529;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1119, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2529:;
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
			    if (gimple_simplify_412 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_POW))
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
	  if (gimple_simplify_413 (res_op, seq, valueize, type, captures, CFN_POW, CFN_EXP, CFN_LOG, CFN_EXP2, CFN_LOG2))
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
      if (gimple_simplify_414 (res_op, seq, valueize, type, captures, CFN_POW))
        return true;
    }
  }
  return false;
}

bool
gimple_simplify_MAX_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_491 (res_op, seq, valueize, type, captures, MAX_EXPR))
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_492 (res_op, seq, valueize, type, captures, MAX_EXPR))
			  return true;
		      }
		    }
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			if (gimple_simplify_492 (res_op, seq, valueize, type, captures, MAX_EXPR))
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q31 };
			if (gimple_simplify_493 (res_op, seq, valueize, type, captures, MAX_EXPR))
			  return true;
		      }
		    }
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q30 };
			if (gimple_simplify_493 (res_op, seq, valueize, type, captures, MAX_EXPR))
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
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			if (gimple_simplify_502 (res_op, seq, valueize, type, captures))
			  return true;
		      }
		    }
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			if (gimple_simplify_502 (res_op, seq, valueize, type, captures))
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
	      case MIN_EXPR:
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
			if (gimple_simplify_502 (res_op, seq, valueize, type, captures))
			  return true;
		      }
		    }
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q31, _p0 };
			if (gimple_simplify_502 (res_op, seq, valueize, type, captures))
			  return true;
		      }
		    }
	          break;
	        }
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (gimple_simplify_503 (res_op, seq, valueize, type, captures))
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
			if (gimple_simplify_503 (res_op, seq, valueize, type, captures))
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
    if (INTEGRAL_TYPE_P (type)
 && TYPE_MAX_VALUE (type)
 && operand_equal_p (captures[1], TYPE_MAX_VALUE (type), OEP_ONLY_CONST)
)
      {
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2598;
	{
	  tree tem;
	  tem = captures[1];
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1149, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail2598:;
      }
    else
      {
	if (INTEGRAL_TYPE_P (type)
 && TYPE_MIN_VALUE (type)
 && operand_equal_p (captures[1], TYPE_MIN_VALUE (type), OEP_ONLY_CONST)
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2599;
	    {
	      tree tem;
	      tem = captures[0];
	      res_op->set_value (tem);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1150, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2599:;
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
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2600;
				      {
					tree tem;
					tem = captures[1];
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1151, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2600:;
				    }
				  else
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2601;
				      {
					tree tem;
					tem = captures[0];
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1152, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2601:;
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
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2602;
				      {
					tree tem;
					tem = captures[1];
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1151, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2602:;
				    }
				  else
				    {
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2603;
				      {
					tree tem;
					tem = captures[0];
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1152, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2603:;
				    }
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
								    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
								    if (gimple_simplify_496 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
							  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
							  if (gimple_simplify_496 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
						    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
						    if (gimple_simplify_497 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
					  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
					  if (gimple_simplify_497 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
							  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
							  if (gimple_simplify_496 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
						tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
						if (gimple_simplify_496 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
					  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
					  if (gimple_simplify_497 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
				if (gimple_simplify_497 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
						    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _q30 };
						    if (gimple_simplify_498 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
					  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _q30 };
					  if (gimple_simplify_498 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _p1 };
				    if (gimple_simplify_499 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _p1 };
			  if (gimple_simplify_499 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2604;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2604;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2604:;
				      }
				  }
			          break;
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
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
				    if (gimple_simplify_500 (res_op, seq, valueize, type, captures, MAX_EXPR, MIN_EXPR))
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
 && (TREE_CODE_CLASS (MAX_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2605;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2605;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2605;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2605:;
					  }
				      }
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
 && (TREE_CODE_CLASS (MAX_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2606;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2606;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2606;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2606:;
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
					  if (gimple_simplify_498 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _q30 };
				if (gimple_simplify_498 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _p1 };
			  if (gimple_simplify_499 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _p1 };
		if (gimple_simplify_499 (res_op, seq, valueize, type, captures, MAX_EXPR, GT_EXPR))
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
 && (TREE_CODE_CLASS (MAX_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2607;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2607;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2607;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2607:;
		      }
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  if (gimple_zero_one_valued_p (_p0, valueize))
    {
      if (gimple_zero_one_valued_p (_p1, valueize))
	{
	  {
	    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	    if (gimple_simplify_501 (res_op, seq, valueize, type, captures, MAX_EXPR, BIT_IOR_EXPR))
	      return true;
	  }
        }
    }
  return false;
}

bool
gimple_simplify_UNGE_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
 && (TREE_CODE_CLASS (UNGE_EXPR) != tcc_comparison
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
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2651;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), UNGE_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2651;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), UNGE_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2651;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2651:;
					  }
				      }
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
 && (TREE_CODE_CLASS (UNGE_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2652;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNGE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2652;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNGE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2652;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2652:;
		      }
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
 && (TREE_CODE_CLASS (UNGE_EXPR) != tcc_comparison
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2653;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNGE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2653;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), UNGE_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2653;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2653:;
		      }
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
	if (gimple_simplify_616 (res_op, seq, valueize, type, captures, UNGE_EXPR))
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
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, UNGE_EXPR, GE_EXPR))
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
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, UNGE_EXPR, UNLE_EXPR))
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
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, UNGE_EXPR, UNLE_EXPR))
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
	  if (gimple_simplify_614 (res_op, seq, valueize, type, captures, UNGE_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_SCALBNL (gimple_match_op *res_op, gimple_seq *seq,
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2667;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1160, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2667:;
	    }
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper code, const tree type, tree _p0, tree _p1)
{
  switch (code.get_rep())
    {
    case BIT_XOR_EXPR:
      return gimple_simplify_BIT_XOR_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case PLUS_EXPR:
      return gimple_simplify_PLUS_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case POINTER_PLUS_EXPR:
      return gimple_simplify_POINTER_PLUS_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case MINUS_EXPR:
      return gimple_simplify_MINUS_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case BIT_IOR_EXPR:
      return gimple_simplify_BIT_IOR_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case POINTER_DIFF_EXPR:
      return gimple_simplify_POINTER_DIFF_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case MULT_EXPR:
      return gimple_simplify_MULT_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case EQ_EXPR:
      return gimple_simplify_EQ_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case NE_EXPR:
      return gimple_simplify_NE_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGNF:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGNF (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGN:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGN (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGNL:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGNL (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGNF16:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGNF16 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGNF32:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGNF32 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGNF64:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGNF64 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGNF128:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGNF128 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGNF32X:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGNF32X (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGNF64X:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGNF64X (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_COPYSIGNF128X:
      return gimple_simplify_CFN_BUILT_IN_COPYSIGNF128X (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_COPYSIGN:
      return gimple_simplify_CFN_COPYSIGN (res_op, seq, valueize, code, type, _p0, _p1);
    case TRUNC_DIV_EXPR:
      return gimple_simplify_TRUNC_DIV_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case CEIL_DIV_EXPR:
      return gimple_simplify_CEIL_DIV_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case FLOOR_DIV_EXPR:
      return gimple_simplify_FLOOR_DIV_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case ROUND_DIV_EXPR:
      return gimple_simplify_ROUND_DIV_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case EXACT_DIV_EXPR:
      return gimple_simplify_EXACT_DIV_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case FLOOR_MOD_EXPR:
      return gimple_simplify_FLOOR_MOD_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case RDIV_EXPR:
      return gimple_simplify_RDIV_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case LT_EXPR:
      return gimple_simplify_LT_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case LE_EXPR:
      return gimple_simplify_LE_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case GT_EXPR:
      return gimple_simplify_GT_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case GE_EXPR:
      return gimple_simplify_GE_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case CEIL_MOD_EXPR:
      return gimple_simplify_CEIL_MOD_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case ROUND_MOD_EXPR:
      return gimple_simplify_ROUND_MOD_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case TRUNC_MOD_EXPR:
      return gimple_simplify_TRUNC_MOD_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case LSHIFT_EXPR:
      return gimple_simplify_LSHIFT_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case RSHIFT_EXPR:
      return gimple_simplify_RSHIFT_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_POWF:
      return gimple_simplify_CFN_BUILT_IN_POWF (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_POW:
      return gimple_simplify_CFN_BUILT_IN_POW (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_POWL:
      return gimple_simplify_CFN_BUILT_IN_POWL (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_POW:
      return gimple_simplify_CFN_POW (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_POWIF:
      return gimple_simplify_CFN_BUILT_IN_POWIF (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_POWI:
      return gimple_simplify_CFN_BUILT_IN_POWI (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_POWIL:
      return gimple_simplify_CFN_BUILT_IN_POWIL (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_HYPOTF:
      return gimple_simplify_CFN_BUILT_IN_HYPOTF (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_HYPOT:
      return gimple_simplify_CFN_BUILT_IN_HYPOT (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_HYPOTL:
      return gimple_simplify_CFN_BUILT_IN_HYPOTL (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_HYPOT:
      return gimple_simplify_CFN_HYPOT (res_op, seq, valueize, code, type, _p0, _p1);
    case BIT_AND_EXPR:
      return gimple_simplify_BIT_AND_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case MIN_EXPR:
      return gimple_simplify_MIN_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case MAX_EXPR:
      return gimple_simplify_MAX_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMINF:
      return gimple_simplify_CFN_BUILT_IN_FMINF (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMIN:
      return gimple_simplify_CFN_BUILT_IN_FMIN (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMINL:
      return gimple_simplify_CFN_BUILT_IN_FMINL (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMINF16:
      return gimple_simplify_CFN_BUILT_IN_FMINF16 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMINF32:
      return gimple_simplify_CFN_BUILT_IN_FMINF32 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMINF64:
      return gimple_simplify_CFN_BUILT_IN_FMINF64 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMINF128:
      return gimple_simplify_CFN_BUILT_IN_FMINF128 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMINF32X:
      return gimple_simplify_CFN_BUILT_IN_FMINF32X (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMINF64X:
      return gimple_simplify_CFN_BUILT_IN_FMINF64X (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMINF128X:
      return gimple_simplify_CFN_BUILT_IN_FMINF128X (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_FMIN:
      return gimple_simplify_CFN_FMIN (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAXF:
      return gimple_simplify_CFN_BUILT_IN_FMAXF (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAX:
      return gimple_simplify_CFN_BUILT_IN_FMAX (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAXL:
      return gimple_simplify_CFN_BUILT_IN_FMAXL (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAXF16:
      return gimple_simplify_CFN_BUILT_IN_FMAXF16 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAXF32:
      return gimple_simplify_CFN_BUILT_IN_FMAXF32 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAXF64:
      return gimple_simplify_CFN_BUILT_IN_FMAXF64 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAXF128:
      return gimple_simplify_CFN_BUILT_IN_FMAXF128 (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAXF32X:
      return gimple_simplify_CFN_BUILT_IN_FMAXF32X (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAXF64X:
      return gimple_simplify_CFN_BUILT_IN_FMAXF64X (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_FMAXF128X:
      return gimple_simplify_CFN_BUILT_IN_FMAXF128X (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_FMAX:
      return gimple_simplify_CFN_FMAX (res_op, seq, valueize, code, type, _p0, _p1);
    case LROTATE_EXPR:
      return gimple_simplify_LROTATE_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case RROTATE_EXPR:
      return gimple_simplify_RROTATE_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case COMPLEX_EXPR:
      return gimple_simplify_COMPLEX_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case UNORDERED_EXPR:
      return gimple_simplify_UNORDERED_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case ORDERED_EXPR:
      return gimple_simplify_ORDERED_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case UNLT_EXPR:
      return gimple_simplify_UNLT_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case UNLE_EXPR:
      return gimple_simplify_UNLE_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case UNGT_EXPR:
      return gimple_simplify_UNGT_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case UNGE_EXPR:
      return gimple_simplify_UNGE_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case UNEQ_EXPR:
      return gimple_simplify_UNEQ_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case LTGT_EXPR:
      return gimple_simplify_LTGT_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_ADD_OVERFLOW:
      return gimple_simplify_CFN_ADD_OVERFLOW (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_SUB_OVERFLOW:
      return gimple_simplify_CFN_SUB_OVERFLOW (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_MUL_OVERFLOW:
      return gimple_simplify_CFN_MUL_OVERFLOW (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_LDEXPF:
      return gimple_simplify_CFN_BUILT_IN_LDEXPF (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_LDEXP:
      return gimple_simplify_CFN_BUILT_IN_LDEXP (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_LDEXPL:
      return gimple_simplify_CFN_BUILT_IN_LDEXPL (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_LDEXP:
      return gimple_simplify_CFN_LDEXP (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_SCALBNF:
      return gimple_simplify_CFN_BUILT_IN_SCALBNF (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_SCALBN:
      return gimple_simplify_CFN_BUILT_IN_SCALBN (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_SCALBNL:
      return gimple_simplify_CFN_BUILT_IN_SCALBNL (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_SCALBLNF:
      return gimple_simplify_CFN_BUILT_IN_SCALBLNF (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_SCALBLN:
      return gimple_simplify_CFN_BUILT_IN_SCALBLN (res_op, seq, valueize, code, type, _p0, _p1);
    case -CFN_BUILT_IN_SCALBLNL:
      return gimple_simplify_CFN_BUILT_IN_SCALBLNL (res_op, seq, valueize, code, type, _p0, _p1);
    case TRUTH_ORIF_EXPR:
      return gimple_simplify_TRUTH_ORIF_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    case TRUTH_OR_EXPR:
      return gimple_simplify_TRUTH_OR_EXPR (res_op, seq, valueize, code, type, _p0, _p1);
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMAL (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_641 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAL))
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
				    if (gimple_simplify_642 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAL))
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
		    if (gimple_simplify_641 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAL))
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
		    if (gimple_simplify_643 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAL))
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
				    if (gimple_simplify_642 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAL))
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
gimple_simplify_CFN_FNMA (gimple_match_op *res_op, gimple_seq *seq,
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
		    if (gimple_simplify_646 (res_op, seq, valueize, type, captures))
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
				    if (gimple_simplify_647 (res_op, seq, valueize, type, captures))
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
		    if (gimple_simplify_646 (res_op, seq, valueize, type, captures))
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2887;
			{
			  res_op->set_op (CFN_FNMS, type, 3);
			  res_op->ops[0] = captures[0];
			  res_op->ops[1] = captures[1];
			  res_op->ops[2] = captures[2];
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1221, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2887:;
		      }
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
				    if (gimple_simplify_647 (res_op, seq, valueize, type, captures))
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
gimple_simplify_VEC_PERM_EXPR (gimple_match_op *res_op, gimple_seq *seq,
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
			      case MINUS_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  switch (TREE_CODE (_p2))
				    {
				    case VECTOR_CST:
				      {
					{
					  tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51, _p2 };
					  if (gimple_simplify_673 (res_op, seq, valueize, type, captures, PLUS_EXPR, MINUS_EXPR))
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
			      case PLUS_EXPR:
			        {
				  tree _q50 = gimple_assign_rhs1 (_a2);
				  _q50 = do_valueize (valueize, _q50);
				  tree _q51 = gimple_assign_rhs2 (_a2);
				  _q51 = do_valueize (valueize, _q51);
				  if (tree_swap_operands_p (_q50, _q51))
				    std::swap (_q50, _q51);
				  switch (TREE_CODE (_p2))
				    {
				    case VECTOR_CST:
				      {
					{
					  tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51, _p2 };
					  if (gimple_simplify_673 (res_op, seq, valueize, type, captures, MINUS_EXPR, PLUS_EXPR))
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
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2889;
			  {
			    tree tem;
			    tem =  op0;
			    res_op->set_value (tem);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1223, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2889:;
			}
		      else
			{
			  if (sel.series_p (0, 1, nelts, 1)
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2890;
			      {
				tree tem;
				tem =  op1;
				res_op->set_value (tem);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1224, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2890:;
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
				      gimple_seq *lseq = seq;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2891;
				      {
					tree tem;
					tem =  t;
					res_op->set_value (tem);
					if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1225, __FILE__, __LINE__, true);
					return true;
				      }
next_after_fail2891:;
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
					      gimple_seq *lseq = seq;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2892;
					      {
						res_op->set_op (BIT_INSERT_EXPR, type, 3);
						res_op->ops[0] =  op0;
						res_op->ops[1] =  ins;
						res_op->ops[2] =  bitsize_int (at * vector_element_bits (type));
						res_op->resimplify (lseq, valueize);
						if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1226, __FILE__, __LINE__, true);
						return true;
					      }
next_after_fail2892:;
					    }
					  else
					    {
					      if (changed
)
						{
						  gimple_seq *lseq = seq;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2893;
						  {
						    res_op->set_op (VEC_PERM_EXPR, type, 3);
						    res_op->ops[0] =  op0;
						    res_op->ops[1] =  op1;
						    res_op->ops[2] =  op2;
						    res_op->resimplify (lseq, valueize);
						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1227, __FILE__, __LINE__, true);
						    return true;
						  }
next_after_fail2893:;
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
				  case VEC_PERM_EXPR:
				    {
				      tree _q30 = gimple_assign_rhs1 (_a2);
				      _q30 = do_valueize (valueize, _q30);
				      tree _q31 = gimple_assign_rhs2 (_a2);
				      _q31 = do_valueize (valueize, _q31);
				      tree _q32 = gimple_assign_rhs3 (_a2);
				      _q32 = do_valueize (valueize, _q32);
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
									    gimple_seq *lseq = seq;
									    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2894;
									    {
									      res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
									      {
										tree _o1[3], _r1;
										_o1[0] = captures[2];
										_o1[1] = captures[3];
										_o1[2] =  op0;
										gimple_match_op tem_op (res_op->cond.any_else (), VEC_PERM_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1], _o1[2]);
										tem_op.resimplify (lseq, valueize);
										_r1 = maybe_push_res_to_seq (&tem_op, lseq);
										if (!_r1) goto next_after_fail2894;
										res_op->ops[0] = _r1;
									      }
									      res_op->resimplify (lseq, valueize);
									      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1228, __FILE__, __LINE__, true);
									      return true;
									    }
next_after_fail2894:;
									  }
								    }
								  }
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
			    }
			  break;
		        default:;
		        }
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
							gimple_seq *lseq = seq;
							if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2895;
							{
							  res_op->set_op (VIEW_CONVERT_EXPR, type, 1);
							  {
							    tree _o1[3], _r1;
							    _o1[0] = captures[2];
							    _o1[1] = captures[3];
							    _o1[2] =  op0;
							    gimple_match_op tem_op (res_op->cond.any_else (), VEC_PERM_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1], _o1[2]);
							    tem_op.resimplify (lseq, valueize);
							    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
							    if (!_r1) goto next_after_fail2895;
							    res_op->ops[0] = _r1;
							  }
							  res_op->resimplify (lseq, valueize);
							  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1228, __FILE__, __LINE__, true);
							  return true;
							}
next_after_fail2895:;
						      }
						}
					      }
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
        }
      break;
    default:;
    }
if (gimple_vec_same_elem_p (_p0, valueize))
  {
    if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p2 };
	  if (types_match (type, TREE_TYPE (captures[0]))
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2896;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1229, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2896:;
	    }
	  else
	    {
	      {
 tree elem = uniform_vector_p (captures[0]);
		  if (elem
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2897;
		      {
			tree tem;
			tem =  build_vector_from_val (type, elem);
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1230, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail2897:;
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
	      case VEC_PERM_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  tree _q32 = gimple_assign_rhs3 (_a1);
		  _q32 = do_valueize (valueize, _q32);
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
								gimple_seq *lseq = seq;
								if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2898;
								{
								  res_op->set_op (VEC_PERM_EXPR, type, 3);
								  res_op->ops[0] = captures[0];
								  res_op->ops[1] = captures[2];
								  res_op->ops[2] =  op0;
								  res_op->resimplify (lseq, valueize);
								  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1231, __FILE__, __LINE__, true);
								  return true;
								}
next_after_fail2898:;
							      }
							    else
							      {
								if (use_2
)
								  {
								    gimple_seq *lseq = seq;
								    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2899;
								    {
								      res_op->set_op (VEC_PERM_EXPR, type, 3);
								      res_op->ops[0] = captures[0];
								      res_op->ops[1] = captures[3];
								      res_op->ops[2] =  op0;
								      res_op->resimplify (lseq, valueize);
								      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1232, __FILE__, __LINE__, true);
								      return true;
								    }
next_after_fail2899:;
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
								gimple_seq *lseq = seq;
								if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2900;
								{
								  res_op->set_op (VEC_PERM_EXPR, type, 3);
								  res_op->ops[0] = captures[1];
								  res_op->ops[1] = captures[4];
								  res_op->ops[2] =  op0;
								  res_op->resimplify (lseq, valueize);
								  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1233, __FILE__, __LINE__, true);
								  return true;
								}
next_after_fail2900:;
							      }
							    else
							      {
								if (use_2
)
								  {
								    gimple_seq *lseq = seq;
								    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2901;
								    {
								      res_op->set_op (VEC_PERM_EXPR, type, 3);
								      res_op->ops[0] = captures[2];
								      res_op->ops[1] = captures[4];
								      res_op->ops[2] =  op0;
								      res_op->resimplify (lseq, valueize);
								      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1234, __FILE__, __LINE__, true);
								      return true;
								    }
next_after_fail2901:;
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
        }
      break;
    default:;
    }
  return false;
}
#pragma GCC diagnostic pop
