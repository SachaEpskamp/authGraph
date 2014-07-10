getGraph <- function(
  object, #authGraph table
  weighted = FALSE
  )
{
  stopifnot(is(object, "authGraph"))

  # Compute Adjacency:
  object$foo <- 1
  
  AuthxPub <- sparseMatrix(object$authorID, object$articleID)
#   AuthxPub[is.na(AuthxPub)] <- 0
  
    authAdj <- 1*(AuthxPub %*% t(AuthxPub))
    rm(AuthxPub)
    
    if (!weighted) authAdj <- 1*(authAdj>0)
    
    rownames(authAdj) <- colnames(authAdj) <- sapply(seq_len(nrow(authAdj)), function(x) object$original_name[object$authorID==x][1])
    

    return(authAdj)
}