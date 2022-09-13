\name{incidence_matrix_from_graph}
\alias{incidence_matrix_from_graph}

\title{
Convert a bipartite graph into an incidence matrix. \cr
}
%~
\description{
Conveert an igraph object into an incidence matrix suitable to be the first argument of the function remove_rows().
}
%~
\usage{
incidence_matrix_from_graph(my_graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_graph}{igraph object}
}
%~
\details{
Required packages: igraph, rjson, dplyr
}
\value{
dataframe, incidence matrix relating two species
%%
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alessandro Vindigni
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(rjson)
library(dplyr)
library(igraph)

base_url <- "https://www.web-of-life.es/"
my_nw_name <- "M_PL_056"
json_url <- paste0(base_url,"get_networks.php?network_name=",my_nw_name)
my_nw <- jsonlite::fromJSON(json_url)
# select the 3 relevant columns and create the igraph object
my_graph1 <- select(my_nw, species1, species2, connection_strength)
my_graph <- graph_from_data_frame(my_graph1)
nw_inc_mat <- incidence_matrix_from_graph(my_graph)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~incidence matrix}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~bipartite networks}% __ONLY ONE__ keyword per line

