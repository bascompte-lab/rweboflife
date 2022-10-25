\name{nestedness}
\alias{nestedness}

\title{
Compute the nestedness of a matrix. \cr
}
%~
\description{
Binarizes the input matrix and computes its nestedness index; nestedness = 1 corresponds to a perfectly nestsed matrix. It assumes the definition of nestedness given in
Fortuna, M.A., et al.: "Coevolutionary dynamics shape the structure of bacteria‐phage infection networks". Evolution 1001-1011 (2019). DOI 10.1111/evo.13731
}
%~
\usage{
nestedness(my_matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_matrix}{matrix, the input matrix. If not yet binarized, it will be converted into a binary matrix}
}
%~
\details{
%%  ~~further deteils, e.g,, required packages~~
}
\value{
Numeric, nestedness value
%%
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alessandro Vindigni, Klementyna Gawecka
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

  n <- 30 # number of rows
  m <- 40 # number of columns

  # random matrix
  M <- matrix(0,n,m)
  for(i in 1:n){
    for (j in 1:m){
      M[i,j] <- floor(runif(1, min = 0, max = 1.2)) # binary random sparse
    }
  }
  # compute the nestedness of M
  nestedness(M)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~nestedness}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~complex networks}% __ONLY ONE__ keyword per line

