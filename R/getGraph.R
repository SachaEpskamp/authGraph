getGraph <- function(
  object, #authGraph table
  weighted = FALSE
  )
{
  stopifnot(is(object, "authGraph"))
  
  # Compute Adjacency:
  object$foo <- 1
  AuthxPub <- acast(object, ID ~ articleID, value.var = "foo")
  AuthxPub[is.na(AuthxPub)] <- 0
  
    authAdj <- 1*(t(AuthxPub) %*% AuthxPub)
    rm(AuthxPub)
    
    if (!weighted) authAdj <- 1*(authAdj>0)
    
    rownames(authAdj) <- colnames(authAdj) <- sapply(seq_len(nrow(authAdj)), function(x) object$original_name[object$ID==x][1])
    

    return(authAdj)
}