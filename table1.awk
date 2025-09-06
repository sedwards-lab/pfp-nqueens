BEGIN {
    print "\\begin{tabular}{S[table-format=2]S[table-format=5]S[table-format=1.2]S[table-format=4]}"
    print "\\toprule"
    print "\\textbf{n} & \\textbf{Solutions} & \\textbf{Time (s)} & \\textbf{Memory (K)} \\\\"
    print "\\midrule"
}
NF == 1 { result = $1 }
NF == 5 { printf("%d & %d & %g & %d \\\\\n", $2, result, $4, $5) }
NF == 7 { printf("%d & %d & %g & %d \\\\\n", $2, result, $6, $7) }

END {
    print "\\bottomrule"
    print "\\end{tabular}"       
}
