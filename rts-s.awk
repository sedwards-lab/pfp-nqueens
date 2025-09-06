BEGIN { print "\\begin{verbatim}" }
NR >= 2 && NR <= 11 { print }
NR == 17 || NR == 18 || (NR >= 20 && NR <= 22) { print }
END  { print "\\end{verbatim}" }
