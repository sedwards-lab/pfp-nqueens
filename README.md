# pfp-nqueens:  Parallel N-Queens in Haskell

This illustrates the sort of parallelization and optimization
you should aim for in the "Parallel Functional Programming" class
taught at Columbia University. <https://www.cs.columbia.edu/~sedwards/classes/2025/4995-fall/index.html>

`nqueens.pdf` is the report, which includes all the code.  This is a
literate program: `nqueens.lhs` can be compiled both by LaTeX and the
Haskell compiler GHC.

The `Makefile` includes rules for generating the PDF report file,
formatting the raw experiment logs (.out and .rts files) into
something suitable for LaTeX or TikZ (using AWK scripts), and
compiling the executables using the [Haskell
Stack](https://docs.haskellstack.org/en/stable/).  `make` by
itself will print a list of targets.

I developed this under Ubuntu Linux; it may also work under MacOS and
Windows.
