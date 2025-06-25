;;%ABCD add to regno 1 2 3 4


(define_attr "length" "" (const_int 0))

(define_constants
  [
   (REG_SP      16)
   (REG_CC      17)
   ])
;; Ordered integral and fixed-point modes of specific sizes.
(define_mode_iterator ALL1 [QI QQ UQQ])
(define_mode_iterator ALL2 [HI HQ UHQ HA UHA])
(define_mode_iterator ALL4 [SI SQ USQ SA USA])
(define_mode_iterator ALL234 [HI SI PSI
                              HQ UHQ HA UHA
                              SQ USQ SA USA])
(define_mode_iterator ALL1234 [QI QQ UQQ
                              HI SI PSI
                              HQ UHQ HA UHA
                              SQ USQ SA USA])
                              
;; Ordered signed integral and signed fixed-point modes of specific sizes.
(define_mode_iterator ALLs1 [QI QQ])
(define_mode_iterator ALLs2 [HI HQ HA])
(define_mode_iterator ALLs4 [SI SQ SA])
(define_mode_iterator ALLs234 [HI SI PSI
                               HQ HA SQ SA])
(define_mode_iterator QIHI  [QI HI])
(define_mode_iterator QIHI2 [QI HI])
(define_mode_iterator QISI  [QI HI PSI SI])
(define_mode_iterator QISI2 [QI HI PSI SI])
(define_mode_iterator QIDI  [QI HI PSI SI DI])
(define_mode_iterator QIPSI [QI HI PSI])
(define_mode_iterator HISI  [HI PSI SI])
(define_mode_iterator HI_SI [HI SI])

(define_mode_iterator SPLIT34 [SI SF PSI
                               SQ USQ SA USA])
(define_code_iterator bitop [xor ior and])

(define_mode_attr SIZE [(QI "1") (QQ "1") (UQQ "1")
                        (HI "2") (HQ "2") (UHQ "2") (HA "2") (UHA "2")
                        (PSI "3")
                        (SI "4") (SQ "4") (USQ "4") (SA "4") (USA "4") (SF "4")])
                        
(define_code_iterator any_extend  [sign_extend zero_extend])
(define_code_iterator any_extend2 [sign_extend zero_extend])


(include "md/constraints.md")
(include "md/predicates.md")
(include "md/arith/arith.md")
(include "md/move/data_transmit.md")
