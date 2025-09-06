NF == 7 { gsub(/-N/, "", $5)
    if ($5 == 1)
	baseline = $6
    printf("%d %.3g\n", $5, baseline/$6)
}
