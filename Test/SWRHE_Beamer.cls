\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SWRHE_Beamer}
[2021/01/11 Beamer Class for School of Water Resources and Hydropower Engineering, Wuhan University]
\LoadClass[compress, t]{beamer}
\RequirePackage[UTF8]{ctex}
\RequirePackage[printwatermark=false]{xwatermark}
\RequirePackage{calc}
\RequirePackage{mdwlist}
\RequirePackage{multirow}
\RequirePackage{cancel}
\RequirePackage{adjustbox}
%\RequirePackage{hyperref}
\RequirePackage{pgf}
\RequirePackage{tikz}
\RequirePackage{tkz-euclide}

\RequirePackage{pgfplots}
%\pgfplotsset{compat=1.16}
%\usepgfplotslibrary{external}
%\tikzexternalize% activate externalization!

\RequirePackage[customcolors,beamer]{hf-tikz}
\RequirePackage{amsmath, amssymb, amsfonts, mathrsfs}
\RequirePackage{media9}
\RequirePackage{caption}
\RequirePackage{graphicx}
\RequirePackage{float}
\RequirePackage{subfig}
\RequirePackage{bm}
\RequirePackage{fontspec}
\RequirePackage{color, xcolor}
\setsansfont[Ligatures=TeX]{DejaVu Sans}
\setCJKsansfont{Noto Serif CJK SC}

\usetikzlibrary{arrows}
\usetikzlibrary{shapes}
\usetikzlibrary{plotmarks}
\usetikzlibrary{intersections}
\usetikzlibrary{calc}
%\usetikzlibrary{3D}
\usetikzlibrary{shadows}
%\usetkzobj{all}
\makeatletter
\tikzoption{canvas is plane}[]{\@setOxy#1}
\def\@setOxy O(#1,#2,#3)x(#4,#5,#6)y(#7,#8,#9)%
  {\def\tikz@plane@origin{\pgfpointxyz{#1}{#2}{#3}}%
   \def\tikz@plane@x{\pgfpointxyz{#4}{#5}{#6}}%
   \def\tikz@plane@y{\pgfpointxyz{#7}{#8}{#9}}%
   \tikz@canvas@is@plane
  }
\makeatother 


% set hf-tikz color
 \hfsetfillcolor{green!5}
 \hfsetbordercolor{blue}
\tikzset{
  h1/.style={
    above left offset={-0.1,1.15},
    below right offset={0.1,-0.95},
    set fill color=green!5,
    set border color=blue,
  },
  h2/.style={
    above left offset={-0.1,0.6},
    below right offset={0.1,-0.45},
    set fill color=green!5,
    set border color=blue,
  },
  h3/.style={
    above left offset={-0.1,0.3},
    below right offset={0.1,-0.25},
    set fill color=green!5,
    set border color=blue,
  }

}

\usetheme{CambridgeUS}
\usefonttheme[onlymath]{serif}
\usecolortheme{dolphin}

\setbeamercolor{uppercol}{fg=white,bg=blue!50!black}
\setbeamercolor{lowercol}{fg=black,bg=green!10!white}
\setbeamertemplate{blocks}[rounded][shadow=true]
\newenvironment{tightcenter}{%
  \setlength\topsep{0pt}
  \setlength\parskip{0pt}
  \begin{center}
}{%
  \end{center}
}


\captionsetup[subfigure]{labelformat=empty}

\addtobeamertemplate{frametitle}{}{%
\vspace*{-0.5cm}
\begin{tikzpicture}[remember picture,overlay]
\node[anchor=north east,yshift=-0.33cm] at (current page.north east)
  {\includegraphics[height=0.84cm]{logo.jpg}};
\end{tikzpicture}
}

\setbeamertemplate{itemize item}{%
       \begin{tikzpicture}
           \shade[ball color=blue!50!white, preaction={fill=black,
opacity=.25,transform canvas={xshift=1mm,yshift=-1mm, yscale=0.5}}] (0,0) circle (0.6ex);
       \end{tikzpicture}
}

\setbeamertemplate{itemize subitem}{%
       \begin{tikzpicture}
           \shade[ball color=red!50!white, preaction={fill=black,
           opacity=.25,transform canvas={xshift=1mm,yshift=-1mm, yscale=0.5}}] (0,0) circle (0.6ex);
       \end{tikzpicture}
}


\setbeamertemplate{enumerate item}[square]

\endinput
