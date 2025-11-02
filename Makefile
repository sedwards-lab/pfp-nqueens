default :
	@echo "make report             Build nqueens.pdf from source and experimental logs"
	@echo "make install-parallel   Install Control.Parallel using Stack"
	@echo "make executables        Build executables for experiments"
	@echo "make clean              Deletes report, executables, and generated results files, but not experimental logs" 

report : nqueens.pdf

executables : install-parallel nqueens-O0 nqueens-O2 nqueens-threaded

# Generate the .pdf file from the LaTeX source and
# processed experimental data
#
# This uses the LaTeX "chessboard" package,
# which, on Ubuntu, can be installed by
#
#    apt install texlive-games

# These files are generated from the logs produced by running the
# experiments (see the nqueens script)
PROCESSED_RESULTS = \
	zaphod4/seqlist-O0.tex \
\
	zaphod4/seqlist-O2.tex \
	zaphod/seqlist-O2-12-14.tex \
	ford/seqlist-O2-12-14.tex \
\
	zaphod4/seqset-O2.tex \
	zaphod/seqset-O2-12-14.tex \
	ford/seqset-O2-12-14.tex \
\
	zaphod4/seqiset-O2.tex \
	zaphod4/seqiset-threaded.tex \
	zaphod4/seqiset2-threaded.tex \
\
	zaphod4/seqlist-threaded.stats.tex \
	zaphod4/seqset-threaded.stats.tex \
	zaphod4/seqiset-threaded.stats.tex \
\
	zaphod4/pariset1-14.dat \
	zaphod/pariset1-14.dat \
	ford/pariset1-14.dat \
\
	zaphod4/pariset2-14.dat \
	zaphod/pariset2-14.dat \
	ford/pariset2-14.dat \
\
	zaphod4/pariset2-14-A4M.stats.tex \
	zaphod4/pariset2-14-A64M.stats.tex

nqueens.pdf : nqueens.lhs $(PROCESSED_RESULTS) zaphod4/pariset1-n6.png
	pdflatex --halt-on-error nqueens.lhs

# Transform raw experimental data files (.out and .rts)
# into result files that will be included in the document

%.tex : %.out table1.awk
	awk -f table1.awk $< > $@

zaphod4/pariset2-14-A4M.stats.tex : zaphod4/pariset2-14-A4M.stats.out rts-s2.awk
	awk -f rts-s2.awk $< > $@

zaphod4/pariset2-14-A64M.stats.tex : zaphod4/pariset2-14-A64M.stats.out rts-s2.awk
	awk -f rts-s2.awk $< > $@

%.stats.tex : %.stats.out rts-s.awk
	awk -f rts-s.awk $< > $@

%.stats.tex : %.rts rts-s.awk
	awk -f rts-s.awk $< > $@

%.dat : %.out plot2.awk
	awk -f plot2.awk $< > $@

# Build executables for the experiments
# Note: the Control.Parallel package needs to be installed
# Run "make install-parallel" to do this

RESOLVER = --resolver=lts-22.33

install-parallel :
	stack install $(RESOLVER) parallel

nqueens-O0 : nqueens.lhs
	stack ghc $(RESOLVER) -- -o nqueens-O0 -Wall nqueens.lhs

nqueens-O2 : nqueens.lhs
	stack ghc $(RESOLVER) -- -o nqueens-O2 -O2 -Wall nqueens.lhs

nqueens-threaded : nqueens.lhs
	stack ghc $(RESOLVER) -- -o nqueens-threaded -O2 -Wall -rtsopts -threaded nqueens.lhs


# Remove all generated files

.PHONY : clean

clean :
	rm -rf nqueens-O0 nqueens-O2 nqueens-threaded nqueens.pdf \
	*.o *.hi *.log *.aux *.out $(PROCESSED_RESULTS)
