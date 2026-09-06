/* Target Code for gaspard
   Copyright (C) 2008-2014 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "regs.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "stor-layout.h"
#include "calls.h"
#include "conditions.h"
#include "output.h"
#include "expr.h"
#include "builtins.h"
/* This file should be included last.  */
#include "target-def.h"
int gaspard_comp_type_attributes (const tree_node *type1, const tree_node *type2) {
	
	printf("gaspard_comp_type_attributes\n");
	
	}
 void  gaspard_expand_prologue (void){
	 	printf("gaspard_expand_prologue\n");

 }
 void  gaspard_expand_epilogue (void) {
	 
	 	 	printf("gaspard_expand_epilogue\n");

	}
 int   gaspard_initial_elimination_offset (int i, int a) {
	 	 	 	printf("gaspard_initial_elimination_offset\n");

	 
	 }
 void  gaspard_print_operand (FILE *fp, rtx rt, int i){
	 	 printf("gaspard_print_operand \n");

	 
	 }
 void  gaspard_print_operand_address (FILE * fp, rtx rt) {
	 
	 printf("print oprand adress \n");
	}
unsigned long gaspard_ret_register (void)
{
  return 31; // toujours 31
}	
rtx gaspard_function_value(const_tree ret_type, const_tree fn_decl_or_type, bool outgoing) {
	
		 printf("gaspard_function_value \n");

	
 return gen_rtx_REG (TYPE_MODE (ret_type), gaspard_ret_register ());

	
}
bool gaspard_function_value_regno_p(const unsigned int regno) {
		 printf("gaspard_function_value_regno_p \n");


	return (regno == 1);
	
}

void gaspard_function_arg_advance (cumulative_args_t cum_v,const function_arg_info &arg){

  CUMULATIVE_ARGS * cum = get_cumulative_args (cum_v);


printf("function_argv_avdence \n");


}
rtx gaspard_function_arg (cumulative_args_t cum_v, const function_arg_info &arg) {

  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
		 printf("gaspard_function_arg \n");

  if (*cum < 31)
    return gen_rtx_REG (arg.mode, *cum);
  else 
    return NULL_RTX;
	
}
static poly_int64
gaspard_return_pops_args (tree, tree, poly_int64)
{
  return 0;
}
static rtx
gaspard_libcall_value (machine_mode mode,
		    const_rtx func ATTRIBUTE_UNUSED)
{
			 printf("gaspard_libcall_value \n");

	
  return gen_rtx_REG (mode,gaspard_ret_register ());
}

static void
gaspard_print_operand_address (FILE * file, machine_mode /*mode*/, rtx addr)
{
				 printf("gaspard_print_operand_address \n");

}
static int
gaspard_address_cost (rtx addr, machine_mode mode ATTRIBUTE_UNUSED,
		   addr_space_t as ATTRIBUTE_UNUSED,
		   bool speed ATTRIBUTE_UNUSED)
{
					 printf("gaspard_address_cost \n");

}
static int
gaspard_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			 reg_class_t from ATTRIBUTE_UNUSED, reg_class_t to)
{
						 printf("gaspard_register_move_cost \n");

	
}
static bool
gaspard_frame_pointer_required (void)
{
							 printf("gaspard_frame_pointer_required \n");

	
	
}

static bool
gaspard_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
	
								 printf("gaspard_can_eliminate \n");

}
static rtx
gaspard_legitimize_address (rtx x, rtx orig_x ATTRIBUTE_UNUSED,
			 machine_mode mode ATTRIBUTE_UNUSED)
{
									 printf("gaspard_legitimize_address \n");

	
}

static bool
gaspard_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED,
			    rtx x ATTRIBUTE_UNUSED)
{
	
										 printf("gaspard_legitimate_constant_p \n");

  return 1;
}

static bool
gaspard_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED,
			   rtx addr, bool strict)
{
											 printf("gaspard_legitimate_address_p \n");

}
static int
gaspard_memory_move_cost (machine_mode mode,
		       reg_class_t rclass ATTRIBUTE_UNUSED,
		       bool in ATTRIBUTE_UNUSED)
{
												 printf("gaspard_memory_move_cost \n");

}

static rtx
gaspard_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
                       int incoming ATTRIBUTE_UNUSED)
{
	printf("gaspard_struct_value_rtx \n");

return NULL;
}
static bool
gaspard_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
 
 
 printf("gaspard_return_in_memory\n");
}
#undef TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES gaspard_comp_type_attributes
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE gaspard_function_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P gaspard_function_value_regno_p
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE 	gaspard_function_arg_advance
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG 	 gaspard_function_arg


#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE 		gaspard_libcall_value

#undef TARGET_RETURN_POPS_ARGS
#define TARGET_RETURN_POPS_ARGS 	gaspard_return_pops_args
/* printing the values.  */
#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND 		gaspard_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS 	gaspard_print_operand_address

/* Relative costs of operations.  */
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST 		gaspard_address_cost
#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST 	gaspard_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST 	gaspard_memory_move_cost

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT	constant_alignment_word_strings
/* Initialize the GCC target structure.  */
#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED	gaspard_frame_pointer_required
#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE 		gaspard_can_eliminate
#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS 	gaspard_legitimize_address
#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P    gaspard_legitimate_constant_p
#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P     gaspard_legitimate_address_p

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false
#undef  TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX		gaspard_struct_value_rtx
#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY		gaspard_return_in_memory

struct gcc_target targetm = TARGET_INITIALIZER;

