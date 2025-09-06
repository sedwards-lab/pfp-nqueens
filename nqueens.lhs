\documentclass{article}
\usepackage[paperheight=8in, paperwidth=6in, 
            hmargin=0.25in, vmargin=0.25in]{geometry}
\usepackage[T1]{fontenc}
\usepackage{libertinus-type1}
\usepackage{libertinust1math}

\usepackage[scaled=0.85]{beramono} % scaled=0.85 basewidth=1.1ex

% Other typewriter font options
% https://tug.org/FontCatalogue/typewriterfonts.html

%\usepackage{inconsolata}
%\usepackage[scaled=0.85]{luximono}  % Serifs
%\usepackage[defaultmono]{droidsansmono}  % No good bold?
%\usepackage[scaled=0.80]{DejaVuSansMono}
%\renewcommand*\ttdefault{txtt}
%\usepackage[scaled=0.85]{newtxtt}  % scaled=0.85 basewidth=1.14ex

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

\lstnewenvironment{code}{\lstset{language=Haskell}}{}

\usepackage{titlesec}
\titleformat{\section}{\sffamily\bfseries}{\thesection}{10pt}{\sffamily\bfseries}


\title{Parallel N-Queens in Haskell}

\author{Stephen A. Edwards, Columbia University}

\begin{document}

\maketitle

Our goal will be to calculate the number of solutions for a board of a
particular size.  We'll adopt a backtracking depth-first search
algorithm due to Niklaus Wirth\footnote{Niklaus Wirth. \emph{Algorithms + Data Structures = Programs}. Prentice Hall, 1976. Section 3.5} that
adds one queen per column in any row that isn't already occupied and
not under diagonal threat from an existing queen.

This eliminates I/O bottlenecks since the input and output of the
algorithm is each a small integer.

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

  \mbox{}\vspace{-8pt}
  
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

\section{Imports}

This is a Literate Haskell file (\texttt{.lhs}) which is a \LaTeX{}
file that \texttt{ghc} can also compile.  It only sees the source
code, which needs to begin with import directives:

% stack install --resolver=lts-22.33 parallel

\begin{code}
import System.Environment(getArgs)
import System.Exit(die)
import qualified Data.Set as Set
import qualified Data.IntSet as IS
import Control.Parallel.Strategies(using, parList, rseq)
-- import Debug.Trace
\end{code}

\newpage

\section{Platforms}

I ran my implementation on three different computers with Intel
\textsc{cpu}s to test its platform sensitivity.  These are the '3820,
an older desktop, the '9700, a newer desktop, and the '4214, a server.

% https://www.cpu-world.com/CPUs/Core_i7/Intel-Core%20i7-3820.html
% https://www.cpu-world.com/CPUs/Core_i7/Intel-Core%20i7%20i7-9700.html
% https://www.cpu-world.com/CPUs/Xeon/Intel-Xeon%204214.html

% https://www.techpowerup.com/cpu-specs/core-i7-3820.c961
% https://www.techpowerup.com/cpu-specs/core-i7-9700.c2183

% https://www.cpubenchmark.net/cpu.php?cpu=Intel+Core+i7-3820+%40+3.60GHz&id=8
% https://www.cpubenchmark.net/cpu.php?cpu=Intel+Core+i7-9700+%40+3.00GHz&id=3477
% https://www.cpubenchmark.net/cpu.php?cpu=Intel+Xeon+Silver+4214+%40+2.20GHz&id=3535

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
  Core &
  Core &
  Xeon Silver \\
  Model &
  i7-3820 & i7-9700 & 4214 \\
  \midrule
  Total Threads & 8 & 8 & 48 \\
  Cores & 4 & 8 & 12 \\
  Threads/Core & 2 & 1 & 2 \\
  Sockets & 1 & 1 & 2 \\
  \midrule
  Frequency (GHz) & 3.6 & 3.0 & 2.2 \\
  Released & 2012 & 2019 & 2019 \\
  Technology (nm) & 32 & 14 & 14 \\
  Socket & LGA2011 & LGA1151 & LGA3647 \\
  Single-thread Rating$^*$ & 1746 & 2774 & 1776 \\
  \midrule
  L1 caches$^\dagger$ & $32\text{K} \times 4$ & $32\text{K} \times 8$ & $32\text{K} \times 24$ \\
  L2 cache & $256\text{K} \times 4$ & $256\text{K} \times 8$ & $1\text{M} \times 24$ \\
  L3 cache & 10M & 12M & $16.5\text{M} \times 2$ \\
  Memory & 64G & 64G & 152G \\
  Speed (GT/s) & 1.6 & 2.1 & 2.1 \\
  Channels$^\ddagger$ & 4 & 2 & 8 \\
  \bottomrule
\end{tabular}

$^*$From \url{https://www.cpubenchmark.net}

$^\dagger$Instruction and Data caches separate; numbers across
all sockets

$^\ddagger$Present on motherboard
  \end{minipage}
\end{center}

While the older '3820 has the clock rate advantage, threads on the
'9700 have twice the available cache memory because it does not use
simultaneous multithreading (Intel's Hyperthreading, which runs
multiple threads per core).  The '3820 was a higher-end chip than the
'9700, but the '3820 is seven years older.  One key difference is the
number of pins, which is driven by the number of external memory
channels and hence bandwidth.

The '4214 is a typical server: slow, but scales up.  It contains two
processor chips, each with four external memory channels (the chips
have six; my motherboard only exposes four) that can be accessed by
both chips.  Its clock rate is slower, but its two chips hold
processors equivalent to six '3820s and can support up to~2~TB of 
memory, compared to~128~GB on the '3820 and '9700.


\newpage

\section{Sequential with lists}

Our first sequential implementation of the backtracking algorithm will
use simple lists for both the row of each queen as well as the sets of
diagonals, which we will pass as arguments.

\begin{code}
nqueens :: String -> Int -> Int
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

\noindent
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/nqueens-seqlist-O0}

'9700, lists, No optimization

\medskip

\parindent=1.5em

Time and memory statistics collected with
\texttt{/usr/bin/time -f "\%C \%e \%M"}

Not surprisingly, enabling optimization provided a noticable, fairly
uniform speedup (roughly a factor of two).

Reassuringly, these solution counts match those on the Wikipedia
Page\footnote{\url{https://en.wikipedia.org/wiki/Eight_queens_puzzle}}.

The '9700 platform is the fastest; the other two are roughly 50\% slower.

\end{minipage}\hfill%
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/nqueens-seqlist-O2}

'9700, lists, \texttt{-O2}

\medskip

\input{zaphod/nqueens-seqlist-O2}

'3820, lists,  \texttt{-O2}

\medskip

\input{ford/nqueens-seqlist-O2}

'4214, lists,  \texttt{-O2}

\end{minipage}

\newpage

\section{Sequential with Sets}

While convenient and efficient for insertions, linked lists are slow
to search.  Instead, we will use the tree-based \texttt{Data.Set}
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
  \input{zaphod4/nqueens-seqlist-O2}
  
  '9700, lists, \texttt{-O2}

  \medskip

  \input{zaphod/nqueens-seqlist-O2}

  '3820, lists,  \texttt{-O2}

  \medskip
  
  \input{ford/nqueens-seqlist-O2}

  '4214, lists,  \texttt{-O2}

\end{minipage}\hfill
\begin{minipage}[t]{0.48\textwidth}
  \input{zaphod4/nqueens-seqset-O2}

  '9700, \texttt{Set}, \texttt{-O2}

  \medskip

  \input{zaphod/nqueens-seqset-O2}

  '3820, \texttt{Set},  \texttt{-O2}

  \medskip
  
  \input{ford/nqueens-seqset-O2}
  
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
\texttt{Data.IntSet} container is specially tailored to integer keys,
so we will try them instead.

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
\input{zaphod4/nqueens-seqset-O2}

'9700, \texttt{Set}, \texttt{-O2}
\end{minipage}\hfill%
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/nqueens-seqiset-O2}

'9700, \texttt{IntSet}, \texttt{-O2}
\end{minipage}

\medskip

Moving from \texttt{Set} to \texttt{IntSet} gave about another $2
\times$ speedup.

Now, in preparation for working on a parallel implementation, let's
check that switching to the multithreaded runtime system (by compiling
with \texttt{-threaded}) does not affect the runtimes.

\medskip

\noindent
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/nqueens-seqiset-O2}

'9700, \texttt{IntSet}, \texttt{-O2}
\end{minipage}\hfill%
\begin{minipage}[t]{0.48\textwidth}
\input{zaphod4/nqueens-seqiset-threaded}

'9700, \texttt{IntSet}, \texttt{-N1}, \texttt{-O2} \texttt{-threaded} \texttt{-rtsopts}
\end{minipage}

\medskip

\newpage

\section{Garbage Collection Statistics for the Sequential Implementations}

\begin{tabular}{ll}
  \toprule
\textbf{list} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/nqueens-seqlist-threaded.stats}
\end{minipage}
\\
\midrule
\textbf{Set} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/nqueens-seqset-threaded.stats}  
\end{minipage}
\\
\midrule
\textbf{IntSet} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/nqueens-seqiset-threaded.stats}  
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

First, we will simply evaluate the first column's choices with
\texttt{`using` parList rseq}.

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

\noindent
\begin{minipage}{0.5\textwidth}
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
    \addplot [only marks,mark=x] table {zaphod4/nqueens-pariset1-14.dat};
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


'9700, $14 \times 14$, \texttt{IntSet}
\end{minipage}%
\begin{minipage}{0.5\textwidth}
  \includegraphics[width=\textwidth]{zaphod4/nqueens-pariset1-n6.png}

'9700, $14 \times 14$, \texttt{IntSet}, \texttt{-N6}
\end{minipage}

\medskip

To verify the anomaly at~6 threads wasn't a sampling artifact, I ran
each test~10 times and plotted each of them; the elapsed times were
remarkably consistent.

The Threadscope graph on the right confirms my suspicions about load
balancing.  While the workload for each guessed row is likely similar,
since we are only creating~14 sparks, with~6 threads, we run the
first~6, then the second~6, then have two ``left over.''

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
    \addplot [only marks,mark=x] table {zaphod/nqueens-pariset1-14.dat};
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


'3820, $14 \times 14$, \texttt{IntSet}
\end{minipage}%

\medskip

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
    \addplot [only marks,mark=x] table {ford/nqueens-pariset1-14.dat};
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


'4214, $14 \times 14$, \texttt{IntSet}
\end{minipage}

\medskip

The anomaly at~6 threads exhibited on the '9700 remains, but now there is
performance plateau from~7 to~13 threads, then a big jump at~14, after
which the speedup actually \emph{decreases}.

Discretization is usually to blame for such non-smooth scaling.  Here,
the big jump at~14 is simply due to this algorithm only producing~14
sparks; parallel resources beyond that are simply left idle; the
slight slowdown after~14 might be due to the increasing cost of
synchronizing more threads.

The conclusion is that we are not supplying the~48-thread machine with enough
parallelism.

\newpage

\section{IntSets 2}
 
In preparing to address the load balancing problem by consolidating
the two \emph{map} operations to parallelize multiple columns, I
inadvertantly sped up the algorithm by another 30\%.  This sequential
code runs over $13\times$ faster than the version that used lists.

\begin{code}
nqueens "seqiset2" n = count (IS.empty, IS.empty, IS.empty)
 where
  count (rows, ups, downs) | column == n = 1
                           | otherwise   = sum $ map count boards
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

\noindent
\begin{minipage}[t]{0.48\textwidth}
\begin{center}
  \input{zaphod4/nqueens-seqiset-threaded}

'9700, \texttt{14 seqiset}, \texttt{-N1}
\end{center}
\end{minipage}\hfill%
\begin{minipage}[t]{0.48\textwidth}
\begin{center}
\input{zaphod4/nqueens-seqiset2-threaded}

'9700, \texttt{14 seqiset2}, \texttt{-N1}
\end{center}
\end{minipage}

\newpage

\section{Parallel IntSets 2}

\begin{code}
nqueens "pariset2" n = count (0 :: Int) (IS.empty, IS.empty, IS.empty)
 where
  count r (rows, ups, downs)
    | column == n = 1
    | r < 2 = sum (map (count (r+1)) boards `using` parList rseq)
    | otherwise   = sum $ map (count (r+1)) boards
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



\noindent
\begin{minipage}{0.5\textwidth}
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
    \addplot [only marks,mark=x] table {zaphod4/nqueens-pariset2-14.dat};
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
\end{minipage}

\medskip

Parallelizing the first two columns' searches eliminated the
anomaly at six threads (the curve is now smooth) also made it slightly
more parallel.

With 8 threads on a $14 \times 14$ board, 140/170 sparks were
converted; the rest fizzled.

\newpage

\noindent
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
    \addplot [only marks,mark=x] table {zaphod/nqueens-pariset2-14.dat};
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

\medskip


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
      ymax=16.4,
      ytick distance=2,
      minor y tick num=1,
      axis y line=left,
      grid=major,
      clip=false
      ]
    \addplot [only marks,mark=x] table {ford/nqueens-pariset2-14.dat};
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

\medskip


\newpage

On the '9700, increasing the nursery on \texttt{nqueens 14 pariset2
  +RTS -N8} unexpectedly slowed things down.  The default is a 4MB
nursery (\texttt{-A5M}); increasing it to 64MB slowed things down
around 9\%.

\medskip

\begin{tabular}{ll}
  \toprule
\texttt{-A4M} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/nqueens-pariset2-14-N8.stats.tex}
\end{minipage}
\\
\midrule
\texttt{-A64M} &
\begin{minipage}{0.7\textwidth}
\scriptsize\input{zaphod4/nqueens-pariset2-14-N8-A64M.stats.tex}
\end{minipage}
\\
\bottomrule
\end{tabular}

\medskip

Increasing the nursery size did reduce garbage collection overhead:
the nursery had to be collected only~16 times instead of~289, reducing
the number of bytes copied during garbage collection as well as the
total time spent performing it (26 to~3~ms), but mutator time increased~62~ms.

This difference is mysterious: There's a small increase (about~3~ms) in
start-up time for the~64MB nursery, but overall, the smaller nursery
looks like it should be slower because it's being interrupted much
more frequently by garbage collection, yet it takes less time to reach
its "ramp-down" phase where the various parallel sparks complete and
the results are aggregated.  Both runs take about the same amount of
time to complete this.




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

\section{Top-Level}

Here is the top-level \emph{main} function, which reads
the board dimensions and runs the appropriate algorithm.

\begin{code}
nqueens _ _ = 0 -- Undefined mode

main :: IO ()
main = do
  args1 <- getArgs
  case args1 of
    [nstr, mode] -> print $ nqueens mode (read nstr)
    _ -> die $ "Usage: nqueens n mode"
\end{code}

\bibliographystyle{plain}
\bibliography{sedwards}

\end{document}
