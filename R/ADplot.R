ADplot <- function(x, filename = "ADplot", clusterID, annotate = FALSE)
{
  eqMat <- outer(x$authorID, x$authorID, '==')
  
  library(qgraph)
  library(igraph)
  
  if (missing(clusterID)){
    coms <- paste("Author",x$authorID)
    clusterID <- x$authorID
  } else {
    coms <- paste("Cluster",clusterID)
  }
  
  edgeList <- which(eqMat, arr.ind = TRUE)
  edgeList <- edgeList[edgeList[,1] > edgeList[,2],]
  
 
  if (annotate){
    
    Graph <- qgraph(edgeList, repulsion = 0.8, DoNotPlot = TRUE, groups = coms, legend = FALSE, esize = 1,
                    nNodes = nrow(x), edgelist = TRUE, directed = FALSE)
    
    qgraphAnnotate(Graph, 
                   Name = x$original_fullName, 
                   Authors =  x$AF,
                   Year =  x$PY,
                   Title =  gsub("[\'\"]","",x$TI),
                   authorID = x$authorID,
                   cluster = clusterID,
                   Address = gsub("[\'\"]","",x$address), fromqgraph = FALSE, filename = filename, image.size = "1200x1200")  
    
  } else {
    
    qgraph(edgeList, repulsion = 0.8, groups = coms, legend = FALSE, esize = 1,
           nNodes = nrow(x), edgelist = TRUE, directed = FALSE)
  }
  
  return(edgeList)
}
