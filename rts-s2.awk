BEGIN { print "\\begin{verbatim}" }
NR >= 2 && NR <= 26 { print }
END  { print "\\end{verbatim}" }
