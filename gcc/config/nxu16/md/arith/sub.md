
;; "subqi3"
;; "subqq3" "subuqq3"
(define_insn_and_split "sub<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                    "=r")
        (minus:ALL1 (match_operand:ALL1 1 "register_operand"           "0")
                    (match_operand:ALL1 2 "nonmemory_or_const_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:ALL1 (match_dup 1)
                               (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sub<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                    "=r")
        (minus:ALL1 (match_operand:ALL1 1 "register_operand"           "0")
                    (match_operand:ALL1 2 "nonmemory_or_const_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sub %0,%2"
  [(set_attr "length" "1")])

;; "subhi3"
;; "subhq3" "subuhq3"
;; "subha3" "subuha3"
(define_insn_and_split "sub<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                    "=r")
        (minus:ALL2 (match_operand:ALL2 1 "register_operand"           "0")
                    (match_operand:ALL2 2 "nonmemory_or_const_operand" "r")))
   ]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:ALL2 (match_dup 1)
                               (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sub<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                    "=r")
        (minus:ALL2 (match_operand:ALL2 1 "register_operand"           "0")
                    (match_operand:ALL2 2 "nonmemory_or_const_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
"sub %A0,%A2\;subc %B0,%B2"
  [(set_attr "length" "2")])


;; "subsi3"
;; "subsq3" "subusq3"
;; "subsa3" "subusa3"
(define_insn_and_split "sub<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                    "=r")
        (minus:ALL4 (match_operand:ALL4 1 "register_operand"           "0")
                    (match_operand:ALL4 2 "nonmemory_or_const_operand" "r")))
   ]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:ALL4 (match_dup 1)
                               (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*sub<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                    "=r")
        (minus:ALL4 (match_operand:ALL4 1 "register_operand"           "0")
                    (match_operand:ALL4 2 "nonmemory_or_const_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
"sub %A0,%A2\;subc %B0,%B2\;sub %C0,%C2\;subc %D0,%D2"
  [(set_attr "length" "4")])
