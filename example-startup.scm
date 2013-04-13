#!/usr/local/bin/guile -s
!#

(use-modules (bot bot))

(define* (into-the-free-node #:key (room "#testing") (pass ""))
  (make-irc-instance girbo "Girbo" "Girbo" "irc.freenode.net" 6697 "irc.freenode.net")
  (do-start-bot girbo #:room room #:pass pass))

(into-the-free-node #:room "#testinggirbo")
