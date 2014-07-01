# Dummy functionj (implement later)

authorDis <- function(x){
  stopifnot(is(x, "authGraph"))
  
  # articleauthors = term I use for author per article. That is, each entry in the data frame.
  
  # Number of articleauthors:
  n <- nrow(x)
  
  # Old ID:
  oldID <- x$authorID
  # New ID:
  newID <- numeric(n)
  
  # Compute shortest lev distance between all authors:
  curname <- strsplit(as.character(x$curname), split = " / ")
  
  # Matrix to indicate if two articleauthors are equal authors:
  eqMat <- matrix(FALSE,n,n)
  diag(eqMat) <- TRUE
  
  # For each pair of articleauthors, check if they are equal or not using van den Akker's algorithm:
  for (i in 1:n)
  {
    for (j in i:n)
    {
      if (i != j)
      {
        ### 1. If lev distance > 3, they are NOT equal:
        dist <- min(adist(curname[[i]],curname[[j]]))
        if (dist > 2)
        {
          next
        }
        
        ### 2. If they share at least on co-author, using old authorID, they are equal:
        # Names of co-authors i:
        #         coi <- curname[x$articleID == x$articleID[i] & seq_len(n) != i]
        
        # Names of co-authors j:
        #         coj <- curname[x$articleID == x$articleID[i] & seq_len(n) != i]
        
        # Check if they are equal:
        #         eqCo <- outer(coi,coj,function(x,y){
        #           mapply(x=x,y=y,FUN=function(x,y){
        #             any( x %in% y) | any(y %in% x)
        #           }, SIMPLIFY = TRUE)  
        #         })
        
        # in a less stupid way using current IDs:
        eqCo <- outer(oldID[x$articleID == x$articleID[i] & seq_len(n) != i],
                      oldID[x$articleID == x$articleID[i] & seq_len(n) != j],
                      '==')
        
        if (any(eqCo))
        {
          eqMat[i,j] <- eqMat[j,i] <- TRUE 
          next
        } 
        
        ### 3. if address is the same, they are equal
        if (x$cleaned_address[i] == x$cleaned_address[j])
        {
          eqMat[i,j] <- eqMat[j,i] <- TRUE 
          next
        } 
        
        # Else we need to check cited refs:
        # Cited refs in i:
        citi <- strsplit(x$CR[i], split = "; ")[[1]]
        
        # Cited refs in j:
        citj <- strsplit(x$CR[j], split = "; ")[[1]]
              
        ### CLEAN UP CITED REFS?
        ### SHOULD INCLUDE DOES i CITE j OR VISE VERSA?
        # Current, do both cite a shared paper:
        eqCit <- outer(citi,
                       citj,
                      '==')
        
        if (any(eqCit))
        {
          eqMat[i,j] <- eqMat[j,i] <- TRUE 
          next
        }         
      }
    }
  }
  
  library(qgraph)
  browser()
  Graph <- qgraph(eqMat, repulsion = 0.8)
  qgraphAnnotate(Graph, Name = x$original_name, fromqgraph = FALSE, filename = "Borsbomen", image.size = "1200x1200")
  
}

# plot(foo, repulsion = 0.8)
