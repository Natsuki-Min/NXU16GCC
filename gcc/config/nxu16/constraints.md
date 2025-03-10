;; Constraint definitions for nxu16
;; Copyright (C) 2009-2021 Free Software Foundation, Inc.

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
;; Constraints
;; -------------------------------------------------------------------------

(define_constraint "A"
  "An absolute address."
  (and (match_code "mem")
       (ior (match_test "GET_CODE (XEXP (op, 0)) == SYMBOL_REF")
	    (match_test "GET_CODE (XEXP (op, 0)) == LABEL_REF")
	    (match_test "GET_CODE (XEXP (op, 0)) == CONST"))))

(define_constraint "Y"
    "An absolute address."
  (ior (match_code "symbol_ref")
       (match_code "label_ref")
       (match_code "const")))

(define_constraint "B"
  "An offset address."
  (and (match_code "mem")
       (match_test "nxu16_offset_address_p (op)")))



(define_constraint "O"
  "The constant zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "k"
  "The constant one"
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "I"
  "An 8-bit constant (0..255)"
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 255")))

(define_constraint "N"
  "A constant -(0..255)"
  (and (match_code "const_int")
       (match_test "ival >= -255 && ival <= 0")))


(define_register_constraint "a" "ALL_REGS"
  "ALL Registers.")
(define_constraint "SP"
  "A Stack Point"
  (and (match_code "reg")
       (match_test "REGNO (op) == REG_SP")))


;; Local Variables:
;; mode: lisp
;; indent-tabs-mode: t
;; End:
