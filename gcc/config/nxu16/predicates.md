;; Predicate definitions for nxu16
;; Copyright (C) 2009-2022 Free Software Foundation, Inc.

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
(define_predicate "nxu16_general_movsrc_operand"
  (match_code "mem,const_int,reg,subreg,symbol_ref,label_ref,const")
{
  /* Any (MEM LABEL_REF) is OK.  That is a pc-relative load.  */
  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == LABEL_REF)
    return 1;

  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == SYMBOL_REF)
    return 1;

  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == REG)
    return 1;

  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == PLUS)
    return 1;

  return general_operand (op, mode);
})

(define_predicate "call_insn_operand"
  (and (match_code "mem")
       (ior (match_test "register_operand (XEXP (op, 0), mode)")
            (match_test "CONSTANT_ADDRESS_P (XEXP (op, 0))"))))

(define_predicate "nxu16_comparison_operator"
    (match_code "eq"))

;; Local Variables:
;; mode: lisp
;; indent-tabs-mode: t
;; End:
