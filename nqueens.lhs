\documentclass{article}
\usepackage[paperheight=8in, paperwidth=6in, 
            hmargin=0.25in, vmargin=0.25in]{geometry}
\usepackage[T1]{fontenc}
\usepackage{libertinus-type1}
\usepackage{libertinust1math}

\usepackage[scaled=0.85]{beramono}

\usepackage[hang,flushmargin]{footmisc}

\usepackage{hyperref}
\hypersetup{
  colorlinks=true,
  linkcolor=red,
  filecolor=blue
}
\usepackage{booktabs}
\usepackage{siunitx}

\usepackage{pgfplots}
\pgfplotsset{width=10cm,compat=1.9}

% apt install texlive-games
\usepackage{chessboard}
\def\myrankformat{%
  \addtocounter{ranklabel}{-1}\arabic{ranklabel}%
  \addtocounter{ranklabel}{1}
}
\def\myfileformat{%
  \addtocounter{filelabel}{-1}\arabic{filelabel}%
  \addtocounter{filelabel}{1}
}

\usepackage{listings}
\lstset{
  columns=flexible,
  basewidth=1.1ex,
  basicstyle=\ttfamily,
  commentstyle={\itshape\color{red}},
}

\lstdefinelanguage{myhaskell}{
        language=Haskell,
        morekeywords={as,die,using,parList,rseq}
    }

\lstnewenvironment{code}{\lstset{language=myhaskell}}{}

\usepackage{titlesec}
\titleformat{\section}{\sffamily\bfseries}{\thesection}{10pt}{\sffamily\bfseries}


\title{Parallel N-Queens in Haskell}

\author{Stephen A. Edwards, Columbia University}

\begin{document}

\maketitle

\noindent

This is intended as a reference project for Columbia's \emph{Parallel
Functional Programming} class,%
\footnote{\url{https://www.cs.columbia.edu/~sedwards/classes/2025/4995-fall/}}
which uses Haskell's
\href{https://hackage.haskell.org/package/parallel/docs/Control-Parallel-Strategies.html}%
{parallel evaluation strategies}, which are described in Marlow's
book.\footnote{Simon Marlow.  \emph{Parallel and Concurrent
Programming in Haskell}.  O'Reilly, 2013.\\
\url{https://simonmar.github.io/pages/pcph.html}} This project starts
from a good ``parallelization candidate''---the n-queens
problem---and, through improvements guided by measurements, ultimately
gave a $12\times$ parallel speedup.  Run time on one machine on a
$14\times14$ board went from~47.5~s to~0.7~s, nearly a~$70\times$
speedup. Roughly $10\times$ was due to sequential improvements and
$7\times$ due to parallelism.

I selected the n-queens problem because I expected it would lead to
successful parallelization.  It is not \textsc{i/o}-limited because
its input (the board size) and output (number of solutions) were both
integers and a modest appetite for runtime memory.  This worked: the
speedup from parallelization peaked at about $12\times$ on a 48-core
server, which Amdahl's law\footnote{Gene M. Amdahl. Validity of the
single processor approach to achieving large scale computing
capabilities. In Proc. Spring Joint Computer Conference (AFIPS),
1967. pp. 483–485. \url{https://doi.org/10.1145/1465482.1465560}} puts
at about 95\% parallel.  The magnitude of the work demanded by this
algorithm can be easily adjusted by changing the board size, which
makes it convenient to set the runtime of a particular experiment to
be long enough to avoid time measurement noise yet short enough to be
practical to run repeatedly. It also did not require a substantial
corpus of input data to be either obtained or synthesized.

After selecting the algorithm, I created a series of implementations,
each faster than the last, by running experiments and adjusting the
implementation to produce an improvement.  I started with a sequential
implementation and twice improved the central data structure used to
track the solution.  Then, I parallelized the algorithm, first
parallelizing the choices for only the first column, then for the
first two columns after seeing load balancing issues.

While reducing total elapsed time is important, scalability---how well
the implementation is able to take advantage of additional parallel
resources---is the deeper, more challenging metric.  To monitor it, I
routinely plot the speedup (elapsed time of a parallel implementation
running on a single thread divided by the elapsed time of a parallel
implementation running on multiple threads) as a function of the
number of threads.  An ideal speedup would be a straight line;
Amdahl's law says a task whose parallel fraction is $0 \leq p \leq 1$
speeds up by $1 /(1 - p + p/n)$ given $n$ parallel resources.

\newpage


\section{N-Queens}

N-Queens is a classical problem\footnote{Dating from 1850. See
W. W. Rouse Ball. \emph{Mathematical Recreations and Essays}.  Macmillan, 1905, 4th ed.\\
\url{https://www.gutenberg.org/files/26839/26839-pdf.pdf}}
that asks how many ways there are to
arrange $n$ queen pieces on an $n\times n$ chessboard such that none
may capture the other, i.e., no two pieces are in the same row,
column, or diagonal.  I will adopt the backtracking depth-first search
algorithm due to Niklaus Wirth\footnote{Niklaus Wirth. \emph{Algorithms + Data Structures = Programs}. Prentice Hall, 1976. Section 3.5} that
adds one queen per column in any row that isn't already occupied and
not under diagonal threat from another queen.

\noindent
\begin{minipage}[t]{0.55\textwidth}
  \parindent=1em

  Wirth represents a partial solution as the row of the queen in each
  column, the set of occupied rows, the set of occupied diagonal rows
  that go up to the right, and the set of occupied diagonals that go
  down to the right.

  We will denote the column (``file'') with $i$ and the row (``rank'')
  with $j$.  A queen at column $i$ and row $j$ is on up-diagonal $i -
  j$, whose values range from $-7$ to $7$; the same queen is on an
  down-diagonal $i + j$, whose values range from~0 to~14.  At right,
  the queen at $(i,j) = (3,2)$ is on up-diagonal $3-2 = 1$ (red) and
  on down-diagonal $3+2 = 5$.

\end{minipage}\hfill%
\begin{minipage}[t]{0.43\textwidth}

  \mbox{}\vspace{-16pt}
  
  \hfill
\chessboard[showmover=false,
  setpieces={Qa8,Qb4,Qc1,Qd3,Qe6,Qf2,Qg7,Qh5},
  hlabelformat=\myrankformat,
  vlabelformat=\myfileformat,
  marginwidth=0pt, % Elimitate gratuitious space around the board
  % up-right diagonals i - j
  pgfstyle=straightmove,
  color=red,
  arrow=,
  markmoves={b1-h7},
  pgfstyle={[base,at={\pgfpoint{-0.5ex}{-0.7ex}}]text},
  text=\small 1, markregion=b1-b1,
  % down-right diagonals i + j
  pgfstyle=straightmove,
  color=blue,
  markmoves={a6-f1},
  pgfstyle={[base,at={\pgfpoint{-0.5ex}{0.3ex}}]text},
  text=\small 5, markregion=a6-a6,
]
\end{minipage}

\vspace{-1\baselineskip}

\section{The \texttt{nqueens} Executable}

This report is a Literate Haskell file (\texttt{.lhs}), which is a
\LaTeX{} file that \textsc{ghc} can also compile.  \textsc{Ghc} only
sees the source code, which begins with import directives:

% stack install --resolver=lts-22.33 parallel

\begin{code}
import System.Environment(getArgs)
import System.Exit(die)
import qualified Data.Set    as Set
import qualified Data.IntSet as IS
import Control.Parallel.Strategies(using, parList, rseq)
\end{code}

I will put our various implementations into a single executable
(compiled three ways) that takes two arguments: the board size and a
string representing which implementation to use.  The \emph{main}
function reads these arguments and dispatches the \emph{nqueens}
function.

\begin{code}
main :: IO ()
main = do
  args1 <- getArgs
  case args1 of
    [nstr, mode] -> print $ nqueens mode (read nstr)
    _            -> die   $ "Usage: nqueens n mode"
\end{code}

\begin{code}
-- nqueens mode board-dimensions -> number-of-solutions
nqueens :: String -> Int -> Int
\end{code}

\newpage

\section{Platforms}

I ran my implementations on three different computers with Intel
\textsc{cpu}s to test its platform sensitivity.  These are the '3820
desktop from 2012, the '9700 desktop from 2019, and '4214 server, also
from 2019.

% https://www.cpu-world.com/CPUs/Core_i7/Intel-Core%20i7-3820.html
% https://www.cpu-world.com/CPUs/Core_i7/Intel-Core%20i7%20i7-9700.html
% https://www.cpu-world.com/CPUs/Xeon/Intel-Xeon%204214.html

% https://www.techpowerup.com/cpu-specs/core-i7-3820.c961
% https://www.techpowerup.com/cpu-specs/core-i7-9700.c2183

% https://www.cpubenchmark.net/cpu.php?cpu=Intel+Core+i7-3820+%40+3.60GHz&id=8
% https://www.cpubenchmark.net/cpu.php?cpu=Intel+Core+i7-9700+%40+3.00GHz&id=3477
% https://www.cpubenchmark.net/cpu.php?cpu=Intel+Xeon+Silver+4214+%40+2.20GHz&id=3535

\bigskip

\begin{center}
  \begin{minipage}{0.75\textwidth}
\begin{tabular}{lrrr}
  \toprule
  &
  \multicolumn{1}{c}{\textbf{'3820}} &
  \multicolumn{1}{c}{\textbf{'9700}} &
  \multicolumn{1}{c}{\textbf{'4214}} \\
  \toprule
  Intel Product Line &
  Core i7 &
  Core i7 &
  Xeon Silver \\
  Model &
  i7-3820 & i7-9700 & 4214 \\
  Year &
  2012 & 2019 & 2019 \\
  \midrule
  Total Threads & 8 & 8 & 48 \\
  Cores & 4 & 8 & 12 \\
  Threads/Core & 2 & 1 & 2 \\
  Sockets & 1 & 1 & 2 \\
  \midrule
  Frequency (GHz) & 3.6 & 3.0 & 2.2 \\
  Technology (nm) & 32 & 14 & 14 \\
  Socket & \textsc{lga}2011 & \textsc{lga}1151 & \textsc{lga}3647 \\
  Single-thread Rating$^*$ & 1746 & 2774 & 1776 \\
  \midrule
  L1 caches$^\dagger$ & $32\text{K} \times 4$ & $32\text{K} \times 8$ & $32\text{K} \times 24$ \\
  L2 cache & $256\text{K} \times 4$ & $256\text{K} \times 8$ & $1\text{M} \times 24$ \\
  L3 cache & 10 MB & 12 MB & $16.5\text{ MB} \times 2$ \\
  Memory & 64 GB & 64 GB & 152 GB \\
  Memory Speed (GT/s) & 1.6 & 2.1 & 2.1 \\
  Memory Channels$^\ddagger$ & 4 & 2 & 8 \\
  \bottomrule
\end{tabular}

$^*$From \url{https://www.cpubenchmark.net}

$^\dagger$Instruction and Data caches separate; numbers across
all sockets

$^\ddagger$Present on motherboard
  \end{minipage}
\end{center}

\bigskip

While the older '3820 has the clock rate advantage, threads on the
'9700 have twice the available cache memory because it does not use
simultaneous multithreading (Intel's Hyperthreading, which runs
multiple threads per core).  The '3820 was a higher-end chip than the
'9700, but the '3820 is seven years older.  One key difference is the
number of pins, which is primarily the number of external memory
channels and hence bandwidth.

The '4214 is a typical server: slow, but scales up.  It contains two
processor chips, each with four external memory channels (the chips
have six; my motherboard only provides slots for four) that can be
accessed by both chips.  Its clock rate is slower, but its two chips
hold processors equivalent to six '3820s and can support up to~2~TB of
memory, compared to~128~GB on the '3820 and '9700.


\newpage

\section{Sequential with Lists}

My first sequential implementation of the backtracking algorithm uses
lists for both the row of each queen as well as the sets of diagonals,
which we will pass as arguments.

\bigskip

\begin{code}
nqueens "seqlist" n = sum $ map (helper [] [] []) [0..n-1]
  where
    helper rows updiags downdiags row
      | row      `elem` rows      ||
        updiag   `elem` updiags   ||
        downdiag `elem` downdiags = 0  -- Can't put it here
      | column == (n - 1)         = 1  -- It fits; we're done
      | otherwise = sum $ map (helper (row      : rows)
                                      (updiag   : updiags)
                                      (downdiag : downdiags)) [0..n-1]
      where column   = length rows -- Number we've already placed
            updiag   = column - row
            downdiag = column + row
\end{code}

\bigskip

\noindent
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/seqlist-O0}

'9700, lists, No optimization

\medskip

\parindent=1.5em

Time and memory statistics collected with
\texttt{/usr/bin/time -f "\%C \%e \%M"}

Not surprisingly, enabling optimization provided a noticable, fairly
uniform speedup (roughly a factor of two).

Reassuringly, these solution counts match those on the Wikipedia
page.\footnotemark

The '9700 platform is the fastest; the other two are roughly half as fast.

\end{minipage}\hfill%
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/seqlist-O2}

'9700, lists, \texttt{-O2}

\medskip

\input{zaphod/seqlist-O2-12-14}

'3820, lists,  \texttt{-O2}

\medskip

\input{ford/seqlist-O2-12-14}

'4214, lists,  \texttt{-O2}

\end{minipage}

\footnotetext{\url{https://en.wikipedia.org/wiki/Eight_queens_puzzle}}

\newpage

\section{Sequential with Sets}

While convenient and efficient for insertions, linked lists are slow
to search.  Instead, I will use the tree-based \texttt{Data.Set}
containers for the three sets.

\begin{code}
nqueens "seqset" n = sum $ map (helper Set.empty Set.empty Set.empty) [0..n-1]
  where
    helper rows updiags downdiags row
      | row      `Set.member` rows      ||
        updiag   `Set.member` updiags   ||
        downdiag `Set.member` downdiags = 0  -- Can't put it here
      | column == (n - 1)               = 1  -- It fits; we're done
      | otherwise = sum $ map (helper (row      `Set.insert` rows)
                                      (updiag   `Set.insert` updiags)
                                      (downdiag `Set.insert` downdiags)) [0..n-1]
      where column   = Set.size rows -- Number we've already placed
            updiag   = column - row
            downdiag = column + row
\end{code}

\noindent
\begin{minipage}[t]{0.48\textwidth}
  \input{zaphod4/seqlist-O2}
  
  '9700, lists, \texttt{-O2}

  \medskip

  \input{zaphod/seqlist-O2-12-14}

  '3820, lists,  \texttt{-O2}

  \medskip
  
  \input{ford/seqlist-O2-12-14}

  '4214, lists,  \texttt{-O2}

\end{minipage}\hfill
\begin{minipage}[t]{0.48\textwidth}
  \input{zaphod4/seqset-O2}

  '9700, \texttt{Set}, \texttt{-O2}

  \medskip

  \input{zaphod/seqset-O2-12-14}

  '3820, \texttt{Set},  \texttt{-O2}

  \medskip
  
  \input{ford/seqset-O2-12-14}
  
  '4214, \texttt{Set},  \texttt{-O2}

\end{minipage}

\medskip

Moving from lists to the \texttt{Set} data structure gave nearly a $3
\times$ speedup on all three platforms.

\medskip


\newpage

\section{Sequential with IntSets}

Haskell's \texttt{Data.Set} only relies on a key being a member of the
\texttt{Ord} class, but our sets only contain small integers.  The
\texttt{Data.IntSet} container is specially tailored to integer keys;
I will try it.

\begin{code}
nqueens "seqiset" n = sum $ map (helper IS.empty IS.empty IS.empty) [0..n-1]
  where
    helper rows updiags downdiags row
      | row      `IS.member` rows      ||
        updiag   `IS.member` updiags   ||
        downdiag `IS.member` downdiags = 0  -- Can't put it here
      | column == (n - 1)              = 1  -- It fits; we're done
      | otherwise = sum $ map (helper (row      `IS.insert` rows)
                                      (updiag   `IS.insert` updiags)
                                      (downdiag `IS.insert` downdiags)) [0..n-1]
      where column   = IS.size rows -- Number we've already placed
            updiag   = column - row
            downdiag = column + row
\end{code}


\noindent
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/seqset-O2}

'9700, \texttt{Set}, \texttt{-O2}
\end{minipage}\hfill%
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/seqiset-O2}

'9700, \texttt{IntSet}, \texttt{-O2}
\end{minipage}

\medskip

Moving from \texttt{Set} to \texttt{IntSet} gave about another $2
\times$ speedup.

Now, in preparation for working on a parallel implementation, I will
verify that switching to the multithreaded runtime system (by compiling
with \texttt{-threaded}) does not affect the runtimes.

\medskip

\noindent
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/seqiset-O2}

'9700, \texttt{IntSet}, \texttt{-O2}
\end{minipage}\hfill%
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/seqiset-threaded}

'9700, \texttt{IntSet}, \texttt{-N1}, \texttt{-O2} \texttt{-threaded} \texttt{-rtsopts}
\end{minipage}

\medskip

\newpage

\section{Garbage Collection Statistics for the Sequential Implementations}

\begin{tabular}{ll}
  \toprule
\textbf{list} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/seqlist-threaded.stats}
\end{minipage}
\\
\midrule
\textbf{Set} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/seqset-threaded.stats}  
\end{minipage}
\\
\midrule
\textbf{IntSet} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/seqiset-threaded.stats}  
\end{minipage}
\\
\bottomrule
\end{tabular}

\noindent
'9700, $13 \times 13$, \texttt{-O2 -threaded -rtsopts}, \texttt{-N1 -s}

\medskip

Here, garbage collection overhead appears very modest, but there are
interesting differences among the three implementations.  While faster
than using lists, using \texttt{Set} allocated nearly 50\% more
memory, needed to copy nearly $5\times$ as much during \textsc{gc},
and ran \textsc{gc} on the nursery (\texttt{Gen 0}) nearly 50\% more
times.  The total \textsc{gc} time nearly doubled.

The \texttt{IntSet} implementation allocated far less memory, ran
\textsc{gc} fewer times, and took less time overall.

Note that in all cases, garbage collection on the main heap
(\texttt{Gen 1}) ran only twice.  This algorithm does not generate
long-lived garbage.

\newpage

\section{Parallel 1}

First, I will just evaluate the first column's choices with
\texttt{`using` parList rseq} and do the rest sequentially.

\bigskip

\begin{code}
nqueens "pariset1" n = sum (firstcol `using` parList rseq)
  where
    firstcol = map (helper IS.empty IS.empty IS.empty) [0..n-1]
    helper rows updiags downdiags row
      | row      `IS.member` rows      ||
        updiag   `IS.member` updiags   ||
        downdiag `IS.member` downdiags = 0  -- Can't put it here
      | column == (n - 1)              = 1  -- It fits; we're done
      | otherwise = sum $
         map (helper (row      `IS.insert` rows)
                     (updiag   `IS.insert` updiags)
                     (downdiag `IS.insert` downdiags)) [0..n-1]
      where column   = IS.size rows -- Number we've already placed
            updiag   = column - row
            downdiag = column + row
\end{code}

\bigskip

\noindent
\begin{minipage}[b]{0.5\textwidth}
\def\p{0.96}

  \begin{tikzpicture}[declare function={amdahl(\n,\p) = 1/(1-\p + \p/\n);}]
    \begin{axis}[
      xlabel={Threads},
      ylabel={Speedup},
      x=12pt,
      y=12pt,
      xmin=0,
      xmax=8.4,
      xtick distance=2,
      minor x tick num=1,
      axis x line=bottom,
      ymin=0,
      ymax=8.4,
      ytick distance=2,
      minor y tick num=1,
      axis y line=left,
      grid=major,
      clip=false
    ]
    \addplot [only marks,mark=x] table {zaphod4/pariset1-14.dat};
    \addplot[
      red,
      domain=1:8,
      samples=8
    ] {x};
    \node [above right,red] at (axis cs:8,8) {Ideal};
    \addplot[
      red,
      domain=1:8
    ] {amdahl(x,\p)};
    \node [red,fill=white,below right] at (axis cs:5,{amdahl(5,\p)}) {$\displaystyle\frac{1}{\displaystyle 1-\p+\frac{\p}{n}}$};
  \end{axis}
  \end{tikzpicture}


'9700, \texttt{14 pariset1}
\end{minipage}%
\begin{minipage}[b]{0.5\textwidth}
  \includegraphics[width=\textwidth]{zaphod4/pariset1-n6.png}

'9700, \texttt{14 pariset1 -N6}
\end{minipage}

\bigskip

To verify the anomaly at~6 threads wasn't a sampling artifact, I ran
each test~10 times and plotted each of them; the elapsed times were
remarkably consistent.

The Threadscope graph on the right confirmed my suspicions that load
balancing was to blame for the anomaly.  While the workload for each
row is likely similar, since we are only creating~14 sparks, with~6
threads, we run the first~6, then the second~6, then have two ``left
over.''

\newpage

I ran this experiment on the other two platforms.  On the '3820,
the maximum speedup (and the level of parallelism, according to Amdahl)
decreased substantially.

\medskip

\noindent
\begin{minipage}{0.5\textwidth}
\def\p{0.8}

  \begin{tikzpicture}[declare function={amdahl(\n,\p) = 1/(1-\p + \p/\n);}]
    \begin{axis}[
      xlabel={Threads},
      ylabel={Speedup},
      x=12pt,
      y=12pt,
      xmin=0,
      xmax=8.4,
      xtick distance=2,
      minor x tick num=1,
      axis x line=bottom,
      ymin=0,
      ymax=4.4,
      ytick distance=2,
      minor y tick num=1,
      axis y line=left,
      grid=major,
      clip=false
    ]
    \addplot [only marks,mark=x] table {zaphod/pariset1-14.dat};
    \addplot[
      red,
      domain=1:4,
      samples=8
    ] {x};
    \node [above right,red] at (axis cs:4,4) {Ideal};
    \addplot[
      red,
      domain=1:8
    ] {amdahl(x,\p)};
    \node [red,fill=white,right] at (axis cs:8,{amdahl(8,\p)}) {$\displaystyle\frac{1}{\displaystyle 1-\p+\frac{\p}{n}}$};
  \end{axis}
  \end{tikzpicture}

'3820, \texttt{14 pariset1}
\end{minipage}%

\bigskip

But the results on the '4214 showed even more anomalies:

\medskip

\noindent
\begin{minipage}{0.9\textwidth}
\def\p{0.97}

  \begin{tikzpicture}[declare function={amdahl(\n,\p) = 1/(1-\p + \p/\n);}]
    \begin{axis}[
      xlabel={Threads},
      ylabel={Speedup},
      x=6pt,
      y=6pt,
      xmin=0,
      xmax=48.4,
      xtick distance=2,
      minor x tick num=1,
      axis x line=bottom,
      ymin=0,
      ymax=14.4,
      ytick distance=2,
      minor y tick num=1,
      axis y line=left,
      grid=major,
      clip=false
      ]
    \addplot [only marks,mark=x] table {ford/pariset1-14.dat};
    \addplot[
      red,
      domain=1:14,
      samples=8
    ] {x};
    \node [above right,red] at (axis cs:14,14) {Ideal};
    \addplot[
      red,
      domain=1:20
    ] {amdahl(x,\p)};
    \node [red,fill=white,right] at (axis cs:20,{amdahl(26,\p)}) {$\displaystyle\frac{1}{\displaystyle 1-\p+\frac{\p}{n}}$};    
  \end{axis}
  \end{tikzpicture}


'4214, \texttt{14 pariset1}
\end{minipage}

\medskip

The anomaly at~6 threads exhibited on the '9700 remains, but now there is a
performance plateau from~7 to~13 threads, then a big jump at~14, after
which the speedup actually \emph{decreases}.

Discretization is usually to blame for such jumps.  Here, the big jump
at~14 is simply due to this algorithm only producing~14 sparks;
parallel resources beyond that are simply left idle; the slight
slowdown after~14 might be due to the increasing cost of synchronizing
more threads.

The conclusion is that this implementation is simply not supplying
the~48-thread machine with enough parallelism.

\newpage

\section{IntSets 2}
 
In preparing to address the load balancing problem by consolidating
the two \emph{map} operations to parallelize multiple columns, I
inadvertantly sped up the algorithm by another 30\%.  This sequential
code runs over $13\times$ faster than the version that used lists.

\bigskip

\begin{code}
nqueens "seqiset2" n = count (IS.empty, IS.empty, IS.empty)
 where
  count (rows, ups, downs) | column == n   = 1
                           | otherwise     = sum $ map count boards
   where
    column = IS.size rows -- Column we're trying to add
    boards = [ ( row  `IS.insert` rows,   -- Next board adds queen at row
                 up   `IS.insert` ups,    -- Record occupied diagonals
                 down `IS.insert` downs )
             | row <- [0..n-1],           -- Consider each possible row
               let up = column - row      -- up-diagonal number
                   down = column + row,   -- down-diagonal number
               row  `IS.notMember` rows,  -- Row may not be occupied
               up   `IS.notMember` ups,   -- Diagonals may not be occupied
               down `IS.notMember` downs ]
\end{code}

\bigskip

\noindent
\begin{minipage}[t]{0.48\textwidth}
\begin{center}
  \input{zaphod4/seqiset-threaded}

'9700, \texttt{14 seqiset -N1}
\end{center}
\end{minipage}\hfill%
\begin{minipage}[t]{0.48\textwidth}
\begin{center}
\input{zaphod4/seqiset2-threaded}

'9700, \texttt{14 seqiset2 -N1}
\end{center}
\end{minipage}

\newpage

\section{Parallel IntSets 2}

My second parallel implementation searches the first two columns in
parallel, then does the rest sequentially.

\bigskip

\begin{code}
nqueens "pariset2" n = count (0 :: Int) (IS.empty, IS.empty, IS.empty)
 where
  count r (rows, ups, downs)
    | column == n  = 1
    | r < 2        = sum (map (count (r+1)) boards `using` parList rseq)
    | otherwise    = sum $ map (count (r+1)) boards
   where
    column = IS.size rows -- Column we're trying to add
    boards = [ ( row  `IS.insert` rows,   -- Next board adds queen at row
                 up   `IS.insert` ups,    -- Record occupied diagonals
                 down `IS.insert` downs )
             | row <- [0..n-1],           -- Consider each possible row
               let up = column - row      -- up-diagonal number
                   down = column + row,   -- down-diagonal number
               row  `IS.notMember` rows,  -- Row may not be occupied
               up   `IS.notMember` ups,   -- Diagonals may not be occupied
               down `IS.notMember` downs ]
\end{code}


\newpage

On the 8-thread '9700, parallelizing the first two columns' searches
eliminated the anomaly at six threads (the curve is now smooth) also
made it slightly more parallel.

With 8 threads on a $14 \times 14$ board, 140/170 sparks were
converted; the rest fizzled.

\bigskip

\noindent
\begin{minipage}[b]{0.45\textwidth}
\def\p{0.97}

  \begin{tikzpicture}[declare function={amdahl(\n,\p) = 1/(1-\p + \p/\n);}]
    \begin{axis}[
      xlabel={Threads},
      ylabel={Speedup},
      x=12pt,
      y=12pt,
      xmin=0,
      xmax=8.4,
      xtick distance=2,
      minor x tick num=1,
      axis x line=bottom,
      ymin=0,
      ymax=8.4,
      ytick distance=2,
      minor y tick num=1,
      axis y line=left,
      grid=major,
      clip=false
    ]
    \addplot [only marks,mark=x] table {zaphod4/pariset2-14.dat};
    \addplot[
      red,
      domain=1:8,
      samples=8
    ] {x};
    \node [above right,red] at (axis cs:8,8) {Ideal};
    \addplot[
      red,
      domain=1:8
    ] {amdahl(x,\p)};
    \node [red,fill=white,below right] at (axis cs:5,{amdahl(5,\p)}) {$\displaystyle\frac{1}{\displaystyle 1-\p+\frac{\p}{n}}$};
  \end{axis}
  \end{tikzpicture}

  '9700, \texttt{14 pariset2}
\end{minipage}\hfill%
\begin{minipage}[b]{0.52\textwidth}
\input{zaphod4/pariset2}

\medskip

'9700, \texttt{pariset2 -N8}
\end{minipage}

\bigskip

On the '3820, the parallelization improved slightly.

\noindent
\noindent
\begin{minipage}{0.5\textwidth}
\def\p{0.8}

  \begin{tikzpicture}[declare function={amdahl(\n,\p) = 1/(1-\p + \p/\n);}]
    \begin{axis}[
      xlabel={Threads},
      ylabel={Speedup},
      x=12pt,
      y=12pt,
      xmin=0,
      xmax=8.4,
      xtick distance=2,
      minor x tick num=1,
      axis x line=bottom,
      ymin=0,
      ymax=4.4,
      ytick distance=2,
      minor y tick num=1,
      axis y line=left,
      grid=major,
      clip=false
    ]
    \addplot [only marks,mark=x] table {zaphod/pariset1-14.dat};
    \addplot[
      red,
      domain=1:4,
      samples=8
    ] {x};
    \node [above right,red] at (axis cs:4,4) {Ideal};
    \addplot[
      red,
      domain=1:8
    ] {amdahl(x,\p)};
    \node [red,fill=white,right] at (axis cs:8,{amdahl(8,\p)}) {$\displaystyle\frac{1}{\displaystyle 1-\p+\frac{\p}{n}}$};
  \end{axis}
  \end{tikzpicture}

'3820, \texttt{14 pariset1}
\end{minipage}%
\begin{minipage}{0.5\textwidth}
\def\p{0.85}

  \begin{tikzpicture}[declare function={amdahl(\n,\p) = 1/(1-\p + \p/\n);}]
    \begin{axis}[
      xlabel={Threads},
      ylabel={Speedup},
      x=12pt,
      y=12pt,
      xmin=0,
      xmax=8.4,
      xtick distance=2,
      minor x tick num=1,
      axis x line=bottom,
      ymin=0,
      ymax=4.4,
      ytick distance=2,
      minor y tick num=1,
      axis y line=left,
      grid=major,
      clip=false
    ]
    \addplot [only marks,mark=x] table {zaphod/pariset2-14.dat};
    \addplot[
      red,
      domain=1:4,
      samples=8
    ] {x};
    \node [above right,red] at (axis cs:4,4) {Ideal};
    \addplot[
      red,
      domain=1:8
    ] {amdahl(x,\p)};
    \node [red,fill=white,right] at (axis cs:8,{amdahl(8,\p)}) {$\displaystyle\frac{1}{\displaystyle 1-\p+\frac{\p}{n}}$};
  \end{axis}
  \end{tikzpicture}


'3820, \texttt{14 pariset2}
\end{minipage}%

\bigskip

The most noticable improvement was on the~48-core '4214, which saw
smooth performance increases up to about~21 threads, after which it
plateaued.

\noindent
\begin{minipage}{0.9\textwidth}
\def\p{0.97}

  \begin{tikzpicture}[declare function={amdahl(\n,\p) = 1/(1-\p + \p/\n);}]
    \begin{axis}[
      xlabel={Threads},
      ylabel={Speedup},
      x=6pt,
      y=6pt,
      xmin=0,
      xmax=48.4,
      xtick distance=2,
      minor x tick num=1,
      axis x line=bottom,
      ymin=0,
      ymax=16.4,
      ytick distance=2,
      minor y tick num=1,
      axis y line=left,
      grid=major,
      clip=false
      ]
    \addplot [only marks,mark=x] table {ford/pariset2-14.dat};
    \addplot[
      red,
      domain=1:16,
      samples=8
    ] {x};
    \node [above right,red] at (axis cs:16,16) {Ideal};
    \addplot[
      red,
      domain=1:30
    ] {amdahl(x,\p)};
    \node [red,fill=white,right] at (axis cs:30,{amdahl(30,\p)}) {$\displaystyle\frac{1}{\displaystyle 1-\p+\frac{\p}{n}}$};
        \addplot[
      red,
      domain=1:48
    ] {amdahl(x,0.92)};
    \node [red,fill=white,right] at (axis cs:48,{amdahl(48,0.9)}) {$\displaystyle\frac{1}{\displaystyle 1-0.92+\frac{0.92}{n}}$};    
  \end{axis}
  \end{tikzpicture}


'4214, \texttt{14 pariset2}
\end{minipage}


\newpage

On the '9700, increasing the nursery with~8 threads unexpectedly
slowed things down.  The default is a~ 4~MB nursery (\texttt{-A4M});
increasing it to~64~MB slowed things down around 9\%.

\medskip

\begin{tabular}{ll}
  \toprule
\texttt{-A4M} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/pariset2-14-A4M.stats.tex}
\end{minipage}
\\
\midrule
\texttt{-A64M} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/pariset2-14-A64M.stats.tex}
\end{minipage}
\\
\bottomrule
\end{tabular}

\medskip

Increasing the nursery size did reduce garbage collection overhead:
the nursery had to be collected only~16 times instead of~292, reducing
the number of bytes copied during garbage collection as well as the
total time spent performing it (28 to~3~ms), but mutator time increased~88~ms.

This difference is mysterious: There's a small increase (about~3~ms) in
start-up time for the~64MB nursery, but overall, the smaller nursery
looks like it should be slower because it's being interrupted much
more frequently by garbage collection, yet it takes less time to reach
its "ramp-down" phase where the various parallel sparks complete and
the results are aggregated.  Both runs take about the same amount of
time to complete this.

\newpage

\section{Conclusion}

\begin{code}
nqueens _ _ = 0 -- Undefined mode
\end{code}


FIXME: Consider vectors, using a Word64

FIXME: heap profiling:
FIXME: stack ghc -- -o nqueens-prof -O2 -Wall -rtsopts -threaded -prof -fprof-auto nqueens.lhs
FIXME: ./nqueens-prof 13 seqiset +RTS -hy -N1 -i0.025
FIXME: hp2ps -c nqueens-prof.hp
FIXME: About 40\% stack, 40\% ARR\_WORDS (unboxed vectors), and 20\% other stuff


FIXME: larger nursery on ford

FIXME: better quantitative analysis of load balancing

FIXME: Heuristic to decide when to run sequential

FIXME: pariset3 to depth of 3


\newpage


\end{document}
