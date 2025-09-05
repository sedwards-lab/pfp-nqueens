HOSTS = zaphod ford zaphod4

RESULTS = \
	zaphod/nqueens-seqlist-O2.tex \
	zaphod/nqueens-seqlist-O2.tex \
	zaphod/nqueens-seqset-O2.tex \
	zaphod/nqueens-pariset1-14.dat \
	zaphod/nqueens-pariset2-14.dat \
	ford/nqueens-seqlist-O2.tex \
	ford/nqueens-seqlist-O2.tex \
	ford/nqueens-seqset-O2.tex \
	ford/nqueens-pariset1-14.dat \
	ford/nqueens-pariset2-14.dat \
	zaphod4/nqueens-seqlist-O0.tex \
	zaphod4/nqueens-seqlist-O2.tex \
	zaphod4/nqueens-seqlist-O2.tex \
	zaphod4/nqueens-seqset-O2.tex \
	zaphod4/nqueens-seqset-O2.tex \
	zaphod4/nqueens-seqiset-O2.tex \
	zaphod4/nqueens-seqiset-O2.tex \
	zaphod4/nqueens-seqiset-threaded.tex \
	zaphod4/nqueens-seqlist-threaded.stats.tex \
	zaphod4/nqueens-seqset-threaded.stats.tex \
	zaphod4/nqueens-seqiset-threaded.stats.tex \
	zaphod4/nqueens-seqiset-threaded.tex \
	zaphod4/nqueens-seqiset2-threaded.tex \
	zaphod4/nqueens-pariset2-14-N8.tex \
	zaphod4/nqueens-pariset2-14-N8-A64M.tex \
	zaphod4/nqueens-pariset1-14.dat \
	zaphod4/nqueens-pariset2-14.dat \
	zaphod4/nqueens-pariset1-n6.png \

nqueens.pdf : nqueens.lhs $(RESULTS)
	pdflatex --halt-on-error nqueens.lhs
