\name{inverse_percolation}
\alias{inverse_percolation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove connected nodes from a network progressively and at random. \cr
}
%~
\description{
This function simulates the secondary extinction of a consumer population (e.g., pollinators) as resources (e.g., plants) are progressively removed.
}
\usage{
remove_rows(bipartite_network, n_iter, strategy, i_seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bipartite_network}{dataframe, incidence matrix relating two species, rows and columns
   }
  \item{n_iter}{integer, number of iterations for randomized removal strategies (see below)
}
  \item{strategy}{string that defines the removal startegy; possible values: \cr
  "RND" = randomly \cr
  "LTM" = from least connected (most specialized) to most connected (most generalist) \cr
  "MTL" = from most connected (most generalist) to least connected (most specialized)
}
   \item{i_seed}{integer, seed of the random sequence}
}
\details{
Required packages: fastmatch, permute
}
\value{
dataframe
%%  If it is a LIST, use
\item{removed_rows}{number of resources removed}
\item{removed_columns}{ average number of consumers that go extinct after having removed a given number of resources}
%% ...
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
library(igraph)
library(dplyr)

## download the a network from the weboflife database as an edge-list in json format
base_url <- "https://www.web-of-life.es/"

my_nw_name <- "M_PL_056"
json_url <- paste0(base_url,"get_networks.php?network_name=",my_nw_name)
my_nw <- jsonlite::fromJSON(json_url)

# select the 3 relevant columns and create the igraph object
my_graph1 <- select(my_nw, species1, species2, connection_strength)
my_graph <- graph_from_data_frame(my_graph1)

my_inc_mat <- incidence_matrix_from_graph(my_graph)

iterations <- 25 # set the number of iterations
removal_strategy <- "RND" # choose the removal strategy

df1 <- remove_rows(my_inc_mat, iterations, removal_strategy, 645)
df <- mutate(df1, "network_name" = my_nw_name, "strategy" = removal_strategy)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~foodwebs}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mutualistic networks}% __ONLY ONE__ keyword per line
\keyword{ ~networks robustness}% __ONLY ONE__ keyword per line

