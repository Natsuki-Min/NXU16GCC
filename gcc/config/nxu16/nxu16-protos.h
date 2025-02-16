/* Prototypes for nxu16.c functions used in the md file & elsewhere.
   Copyright (C) 2009-2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

extern void  nxu16_expand_prologue (void);
extern void  nxu16_expand_epilogue (void);
extern int   nxu16_initial_elimination_offset (int, int);
extern bool  nxu16_offset_address_p (rtx);
extern void  nxu16_split_symbolic_move (rtx, rtx);
extern bool  nxu16_function_arg_regno_p (int);
extern void  nxu16_init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype, rtx libname, tree /*fndecl*/);
extern int  nxu16_regno_mode_ok_for_base_p (int regno, machine_mode mode,bool strict_p);
extern bool nxu16_epilogue_uses (unsigned int regno);
extern  void nxu16_asm_out_dtor (rtx symbol, int priority);
extern  void nxu16_asm_out_ctor (rtx symbol, int priority);
