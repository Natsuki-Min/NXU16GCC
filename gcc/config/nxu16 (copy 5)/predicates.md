;; Predicate definitions for nxu16
;; Copyright (C) 2009-2020 Free Software Foundation, Inc.
;; Contributed by Anthony Green <green@nxu16logic.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; Predicates
;; -------------------------------------------------------------------------

;; Nonzero if OP can be source of a simple move operation.

(define_predicate "symbolic_operand"
  (match_code "mem,const_int,reg,subreg,symbol_ref,label_ref,const")
{
	if(GET_CODE(op) == SYMBOL_REF || GET_CODE(op) == LABEL_REF)
        {
                return 1;
        }
        return 0;
})

(define_predicate "constant_operand"
  (match_code "mem,const_int,reg,subreg,symbol_ref,label_ref,const")
{
        if(GET_CODE(op)!=CONST_INT)
                return 0;
        if(INTVAL(op) <=0xff)
        {
                return 1;
        }
        return 0;
})
(define_predicate "zero_register_operand"
  (match_code "mem,const_int,reg,subreg,symbol_ref,label_ref,const")
{
        if(GET_CODE(op)!=REG)
                return 0;
        if(REGNO(op) == 0)
        {
                return 1;
        }
        return 0;
})
