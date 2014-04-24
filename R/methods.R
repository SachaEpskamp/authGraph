
plot.authGraph <- function(x,...) 
{
#   qgraph(x$Adjacency,...)
  adj <- getGraph(x)
  qgraph(adj,...)
}
