%%    File : inftechrep.cls    (LaTeX2e class file)
%%  Author : Steve Haeck <sjh@dcs.ed.ac.uk>
%% Version : 0.1
%% Updates : 0.1 [13/5/96] - initial release.
%%         : 1.0 [2000/11/28] - various updates
%%                  Got rid of \month and \year commands -- they mess up
%%                      \today
%%                  Use new "eushield" package
%%                  Update copyright statement to use report year
%%                  Use parskip.sty instead of parskip.clo
%%           1.01 [2002/10/11] - Fixed errors found by Claudio Castell
%%                  Changed to be School instead of Division
%%
%% This file contains a class definition, inftechrep, for the LaTeX2e
%% system which defines the layout of technical reports used by the
%% informatics division.
%%
%% For information on this class, please refer to "texdoc inftechrep"

\NeedsTeXFormat{LaTeX2e}[1994/12/01]
\ProvidesClass{inftechrep}[2002/10/11 v1.01 Informatics Technical Report Class]

%% Load the necessary options
\input{informaticsopt.clo} %% Sets the macros up for particular report type
\input{infcopyright.clo}   %% Defines the standard copyright notices
\input{informaticsdef.clo} %% Defines standard informatics wide macros

%% Process the options
\newcommand{\optionerror}[1]
    {\typeout{*** Warning: #1 is not a legal class option}}
\newif\ifCOVERPAGE \newif\ifTITLEPAGE
\DeclareOption{titlepage}{\TITLEPAGEtrue}
\DeclareOption{notitlepage}{\TITLEPAGEfalse}
\DeclareOption{coverpage}{\COVERPAGEtrue}
\DeclareOption{nocoverpage}{\COVERPAGEfalse}
\DeclareOption{twocolumn}{\OptionNotUsed\optionerror{twocolum}}
\DeclareOption{parskip}{\AtBeginDocument{\RequirePackage{parskip}}}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

\ExecuteOptions{12pt,notitlepage,nocoverpage}   % the default options
\ProcessOptions

\LoadClass[a4paper]{article}

%% Load necessary packages
\RequirePackage{a4wide}           % use this for margin settings
\RequirePackage{ifthen}           % used in several macros...
\RequirePackage{graphics}         % For the University shield
\RequirePackage{eushield}         % For the actual shield graphic
\shieldtype{0}  % Monochrome

%% Commands for title page stuff
\def\abstractname{\textbf{Abstract}}

\def\keywords#1{\gdef\@keywords{#1}}
\let\@keywords\relax
\def\keywordsname{\textbf{Keywords}}

\def\inftech@keywords{\par\ifx\@keywords\relax\else
	{\vspace{1.4ex}\noindent\keywordsname~:~\@keywords\par\fi}}
\def\inftech@copyright{\noindent\@cright}

\renewenvironment{abstract}{%
    \ifTITLEPAGE\begin{center}\abstractname\end{center}\noindent
    \else\small\noindent\textbf{\abstractname~:~}\fi}
{\par\inftech@keywords\vspace{3ex}%
    \ifTITLEPAGE\relax\else\inftech@copyright\fi\clearpage}

\renewcommand{\author}[2][]{
   \newcommand{\@author}{#2}
   \ifthenelse{\equal{#1}{}}
   {\newcommand{\@coverauthor}{#2}}
   {\newcommand{\@coverauthor}{#1}}}

\let\@author\relax
\let\@coverauthor\relax

\renewcommand{\title}[2][]{
   \newcommand{\@title}{#2}
   \ifthenelse{\equal{#1}{}}
   {\newcommand{\@covertitle}{#2}}
   {\newcommand{\@covertitle}{#1}}
}
\let\@title\relax
\let\@covertitle\relax

\def\institutes#1{\gdef\@institutes{#1}}
\let\@institutes\relax

\def\reportnumber#1{\gdef\@reportnumber{#1}}
\let\@reportnumber\relax

\def\published#1{\gdef\@published{#1}}
\let\@published\relax

\def\cright#1{\gdef\@cright{#1}}
\let\@cright\inf@copyrightdefault

%% Set the month and year of the report if specified; if not, use current
%% month and year.
\def\reportmonth#1{\def\@reportmonth{\month@num{#1}}}
\def\@reportmonth{\month@num{\month}}
\def\month@num#1{\typeout{#1}%
    \ifcase#1\or January\or February\or March\or
    April\or May\or June\or July\or August\or September\or October\or
    November\or December\or#1\fi}
\def\reportyear#1{\gdef\@reportyear{#1}\copyyear{#1}}
\let\@reportyear\year

\def\@mymaketitle{
  \setcounter{page}{0}
  \begin{center}
    {\LARGE\bfseries\@title\par}\vskip 1.5em
    {\large \@author\\[6pt]
     \ifx\@reportnumber\relax\else 
        Informatics Research Report \@reportnumber\\[1ex]\fi
     \@deptname\\
     \@institutes\\[1ex]
     \cp@monthyear}
  \end{center}%
  \vspace{1.5em}
  \noindent\ifx\@published\relax\else\@published\vspace{3ex}\fi
}

\def\cleartitlevars{
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}


\renewcommand\maketitle{\ifCOVERPAGE\infocoverpage\fi
%  \renewcommand{\thefootnote}{\@fnsymbol\c@footnote}%
%  \let\footnotesize\small\let\footnote\thanks\vspace{3ex}
  \ifTITLEPAGE
    \begin{titlepage}
      \@mymaketitle
      \par\noindent\inftech@copyright
    \end{titlepage}
  \else
    \newpage\@mymaketitle\thispagestyle{empty}
  \fi
  \global\@topnum\z@   % Prevents figures from going at top of page.
%  \@thanks
  \setcounter{footnote}{0}
  \cleartitlevars
  \par
}

%% These are the font changes within the selected family.
%% N.B. biggroup could be larger if we enforce the more compact 
%% adobe times in place of the cmr times.
\newcommand\itr@biggroup{\fontseries{b}\fontshape{n}%
			 \fontsize{17.28}{12}\selectfont}
\newcommand\itr@midgroup{\fontseries{b}\fontshape{n}%
			 \fontsize{14}{16.8}\selectfont}
\newcommand\itr@group{\fontseries{b}\fontshape{n}%
			 \fontsize{12}{14.4}\selectfont}
\newcommand\itr@deptaddress{\fontseries{m}\fontshape{n}%
			 \fontsize{11}{13.2}\selectfont}
\newcommand\itr@number{\fontseries{b}\fontshape{n}%
			 \fontsize{12}{14.4}\selectfont}
\newcommand\itr@deptheading{\fontseries{m}\fontshape{n}%
			 \fontsize{10}{12}\selectfont}
\newcommand\itr@title{\fontseries{b}\fontshape{n}%
			 \fontsize{14}{16.8}\selectfont}
\newcommand\itr@author{\fontseries{m}\fontshape{n}%
			 \fontsize{14}{16.8}\selectfont}
\newcommand\itr@copyr{\fontseries{m}\fontshape{n}%
			 \fontsize{10}{12}\selectfont}

%% Use the month and year given for the report.
\providecommand{\cp@monthyear}{%
  \@reportmonth
  \space
  \number\@reportyear}

%% \infocoverpage makes the generic coverpage. Uses
%% \@coverauthor, \@covertitle & \@reportnumber macros.
%% (Really ought to temporarily set page size to ensure standard. - da)
%%
\newcommand{\infocoverpage}{%
 \begin{titlepage}%
   \baselineskip 12pt%
	\vspace*{-1in}
   \begin{flushright} 
	\scalebox{0.2332}{\includegraphics{\eushield}}
   \end{flushright}
   \itr@biggroup\noindent\cp@infname\hfill\\\rule{\linewidth}{1mm}
   \begin{flushright}
     \itr@midgroup\@institutes
   \end{flushright}
   \vspace*{\stretch{1}}
   \begin{centering}
       \itr@title\@covertitle\\[2ex]
       \itr@author by\\[2ex] \@coverauthor\\
   \end{centering}
   \vspace*{\stretch{1}}
   \itr@group Informatics Research Report \itr@number\@reportnumber\\
   \rule{\linewidth}{1mm}
   \itr@group\cp@infshortname\ \hfill \itr@number\cp@monthyear\\
   \ignorespaces\itr@deptaddress\@infaddress
 \end{titlepage}
}


%% Names etc.
%%

\newcommand{\cp@infname}
  {School of Informatics, University of Edinburgh}

\newcommand{\cp@infshortname}
  {School of Informatics}

\newcommand{\@infaddress}{%
      http://www.informatics.ed.ac.uk/}


