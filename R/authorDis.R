# Dummy functionj (implement later)
# library("igraph")
# library("Matrix")

# 
# 
# # benchStart <- function(x){
#   obj <- paste0("# benchSTART_",x)
#   assign(obj,Sys.time(), envir = .GlobalEnv)
#   
#   diff <- paste0("# benchDIFF_",x)
#   if (!exists(diff)){
#     assign(diff,0, envir = .GlobalEnv)
#   }
# }
# 
# # benchFinish <- function(x){
#   obj0 <- paste0("# benchSTART_",x)
#   diff <- paste0("# benchDIFF_",x)
#   assign(diff,get(diff) + (Sys.time() - get(obj0)), envir = .GlobalEnv)
#   
# }
# 
# # benchReport <- function(x){
#   diff <- paste0("# benchDIFF_",x)
#   cat("Total time for",x,"\n")
#   print(get(diff))
# }




authorDis <- function(x, plotGraph = FALSE, filename = "authorDis", maxIter = 10, returnList = FALSE,
                      verbose = TRUE
){
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
  eqMat <- Matrix::Matrix(FALSE,n,n, sparse = TRUE)
  diag(eqMat) <- TRUE
  
  it <- 1
  
  # Repeat algorithm until convergence:
  repeat{
    if (verbose){
      cat("Starting iteration",it,"\n")
    }
    # benchStart("entire")
    xOld <- x
    
    # For each pair of articleauthors, check if they are equal or not using van den Akker's algorithm:
    ### VECTORIZED CHECKS ###
    
    # Get all combinations:
    combs <- t(combn(1:n,2))
    
    # Check if articleID is same, don't then:
    combs <- combs[x$articleID[combs[,1]] != x$articleID[combs[,2]],]
    
    # Check if lev distance between first names is huge, if so, skip:
    names1 <- sapply(curname,"[[",1)
    
    # Split:
    splitNames1 <- strsplit(names1,split=',')
    last1 <- sapply(splitNames1,'[[',1)
    
    # If lev distance between first names is > 5, don't consider:
    levs <- levVectors(last1[combs[,1]],last1[combs[,2]])
    #     stringdist:::stringdist(names1[combs[,1]],names1[combs[,2]])
    # #     
    combs <- combs[levs < 3,]
    
    pb <- txtProgressBar(0,nrow(combs),style = 3)
    
    for (COMB in seq_len(nrow(combs))){
      
      i <- combs[COMB,1]
      j <- combs[COMB,2]
      
      #     for (i in 1:n)
      #     {
      #       for (j in i:n)
      #       {
      # try(# benchFinish("single"))
        # benchStart("single")
        if (i != j)
        {
          ### IF articleID is the same, they can not be the same person!
          if (x$articleID[i] == x$articleID[j])
          {
            next
          }
          
          
          
          
          ### 1. they are NOT equal if:
          #         # - first initial is not the same
          #         # - one initial is nested in the other
          #         # - lev distance between last names > 1 if nchar < 5, 
          #         cont <- Matrix::Matrix(FALSE,length(curname[[i]]), length(curname[[j]]))
          #         
          #         for (ii in seq_along(curname[[i]]))
          #         {
          #           for (jj in seq_along(curname[[j]]))
          #           {
          #            nami <- strsplit(curname[[i]][[ii]],split=',')[[1]]
          #            namj <- strsplit(curname[[j]][[jj]],split=',')[[1]]
          #            
          #            # First initial equal?
          #            firstInit <- strtrim(nami[2],1) == strtrim(namj[2],1)
          #            
          #            # One initial is nested in the other?
          #            nested <- grepl(nami[2],namj[2]) | grepl(namj[2],nami[2])
          #            
          #            # lev distance last name < 2:
          #            levDist <- adist(nami[1],namj[1]) < 2
          #            
          #            cont[ii,jj] <- firstInit & nested & levDist
          #           }
          #         }
          # benchStart("lev")
          t0 <- Sys.time()
          # New:
          # - lev distance initials > 1
          # - lev distance between last names > 1 if nchar longest name < 5, 2 otherwise
          
          cont <- Matrix::Matrix(FALSE,length(curname[[i]]), length(curname[[j]]))
          
          for (ii in seq_along(curname[[i]]))
          {
            for (jj in seq_along(curname[[j]]))
            {
              nami <- strsplit(curname[[i]][[ii]],split=',')[[1]]
              namj <- strsplit(curname[[j]][[jj]],split=',')[[1]]
              
              # First initial equal?
              #               firstInit <- strtrim(nami[2],1) == strtrim(namj[2],1)
              #               
              #               # One initial is nested in the other?
              #               nested <- grepl(nami[2],namj[2]) | grepl(namj[2],nami[2])
              # lev distance initials max 1:
              FirstName <- adist(nami[2],namj[2]) <= 1
              
              # lev distance last name < 2:
              if (max(nchar(c(nami[1],namj[1]))) < 5)
              {
                LastName <- adist(nami[1],namj[1]) <= 1
              } else {
                LastName <- adist(nami[1],namj[1]) <= 2
              }
              
              cont[ii,jj] <- FirstName & LastName
            }
          }
          t1 <- Sys.time()
          #           if (difftime(t1,t0,units="secs") > 0.1){
          #             
          #             
          #             browser()
          #           }
          
          # benchFinish("lev")
          
          if (!any(cont))
          {
            next
          }
          
          
          ### IF A SHARED EMAIL, EQUAL AND STOP ###
          # benchStart("mail")
          emi <- strsplit(x$EM[i],split="; ")[[1]]
          emi <- emi[emi!='']
          
          emj <- strsplit(x$EM[j],split="; ")[[1]]
          emj <- emj[emj!='']
          # benchFinish("mail")
          if (any(emi %in% emj) | any(emj %in% emi)){
            eqMat[i,j] <- eqMat[j,i] <- TRUE 
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
          # benchStart("co")
          # in a less stupid way using current IDs:
          eqCo <- outer(x$authorID[x$articleID == x$articleID[i] & seq_len(n) != i],
                        x$authorID[x$articleID == x$articleID[j] & seq_len(n) != j],
                        '==')
          # benchFinish("co")
          if (any(eqCo))
          {
            eqMat[i,j] <- eqMat[j,i] <- TRUE 
            next
          } 
          
          
          
          ### 3. if address is the same, they are equal
          # benchStart("Address")
          if (x$cleaned_address[i]!= '' & x$cleaned_address[j]!= '' & x$cleaned_address[i] == x$cleaned_address[j])
          {
            eqMat[i,j] <- eqMat[j,i] <- TRUE 
            # benchFinish("Address")
            next
          } 
          
          
          # ONLY CONTINUE IF ANY NAME MATCHES EXACTLY:
          if (!any(outer(curname[[i]],curname[[j]],'=='))){
            next
          }
          
          
          ################## CITED REFS CHECK ##################
          # benchStart("cit")
          # Cited refs in i:
          citi <- strsplit(x$CR[i], split = "; ")[[1]]
          
          # Cited refs in j:
          citj <- strsplit(x$CR[j], split = "; ")[[1]]
          
          # If any is na, next:
          if (length(citi) == 0 | length(citj) == 0){
            next
            # benchFinish("cit")
          } 
          if (identical(citi, NA) | identical(citj, NA))
          {
            next
            # benchFinish("cit")
          }
          
          # Standardized:
          stdCit <- function(x, nInitials = 100)
          {
            res <- sapply(x, function(x){
              if (!grepl("^.*?\\d{4}",x)) return(NA)
              
              # Only up to year:
              x <- regmatches(x,regexpr("^.*?\\d{4}", x, perl=TRUE))
              
              # Remove lowercase after first space:
              spl <- strsplit(x, split = " ")
              
              # Alle initialen
              #             x <- sapply(spl, function(x) paste(c(x[1],gsub("[a-z]","",x[-1],perl=TRUE)), collapse = " "))
              
              # Eerste twee initialen:
              firstN <- function(x, n){
                if (length(x) < n){
                  return(x)
                } else {
                  return(x[1:n])
                }
              }
              
              x <- sapply(spl, function(x) paste(c(x[1],paste(gsub("[a-z]","",x[-1],perl=TRUE), collapse = "")), collapse = ","))
              
              # Remove punctuation:
              x <- tolower(x)
              
              # Remove spaces and punctuation:
              x <- gsub(pattern="[ \\.-]",replacement="",x)
              
              # Remove all extra letters after comma:
              x <- gsub(paste0("(?<=,[a-zA-Z]{",nInitials,"})[a-zA-Z]?"),"", x, perl = TRUE)
              
              x
            })
            
            unname(res)
          }
          
          stdciti <- stdCit(citi, nInitials = 2)
          stdcitj <- stdCit(citj, nInitials = 2)
          
          
          ### Check if standardized citations are equal:
          eqCitCit <- outer(stdciti,
                            stdcitj,
                            '==')
          eqCitCit[is.na(eqCitCit)] <- FALSE
          
          if (sum(eqCitCit) >2 )
          {
            eqMat[i,j] <- eqMat[j,i] <- TRUE 
            # benchFinish("cit")
            next
          } 
          # Check only for DOI:
          # Extract DOIs:
          
          getDOI <- function(x){
            sapply(seq_along(x), function(iii){
              m <- regexpr("(?<=DOI )[^(DOI)].*?(?=$)", x[iii], perl=TRUE)
              if (m == -1)
                return(NA)
              
              regmatches(x[iii],m)
            })
          }
          
          DOIsi <- getDOI(citi)
          DOIsj <- getDOI(citj)
          
          eqCitDOI <- outer(DOIsi,
                            DOIsj,
                            '==')
          
          eqCitDOI[is.na(eqCitDOI)] <- FALSE
          eqCitCit[is.na(eqCitCit)] <- FALSE
          
          if (all(dim(eqCitCit) > 0) && all(dim(eqCitCit) == dim(eqCitDOI)) && sum(eqCitCit | eqCitDOI) >2 )
          {
            eqMat[i,j] <- eqMat[j,i] <- TRUE 
            # benchFinish("cit")
            next
          } 
          
          
          # If i cites j or j cites i, they are equal:
          # via DOI:
          if ( (x$DI[i] %in% DOIsj && !is.na(x$DI[i]) & x$DI[i] != '') | (x$DI[j] %in% DOIsi & !is.na(x$DI[j]) && x$DI[j] != ''))
          {
            eqMat[i,j] <- eqMat[j,i] <- TRUE 
            # benchFinish("cit")
            next
          }
          
          # Via reconstructed article name:
          firstAuti <- which(x$articleID == x$articleID[i])[1]
          arti <-  paste0(x$cleaned_nameFirstTwoInit[firstAuti],",",x$PY[firstAuti])
          
          firstAutj <- which(x$articleID == x$articleID[j])[1]
          artj <-  paste0(x$cleaned_nameFirstTwoInit[firstAutj],",",x$PY[firstAutj])
          if ( artj %in% stdciti | arti %in% stdcitj )
          {
            eqMat[i,j] <- eqMat[j,i] <- TRUE 
            # benchFinish("cit")
            next
          }         
          
          
          # If one common self reference, they are equal!
          #           allNames <- gsub(",.*","",unlist(curname[c(i,j)]))
          
          # Include self citation of all authors:
          allNames <- gsub(",.*","",unlist(curname[x$articleID %in% x$articleID[c(i,j)]]))
          str <- paste0("(",allNames,")",collapse="|")
          selfciti <- grep(str, citi, ignore.case = TRUE)
          selfcitj <- grep(str, citj, ignore.case = TRUE)
          
          # CHECK FOR EQUAL SELF CITATION
          eqCitCit <- outer(stdciti[selfciti],
                            stdcitj[selfcitj],
                            '==')
          eqCitCit[is.na(eqCitCit)] <- FALSE
          
          DOIsi <- getDOI(citi[selfciti])
          DOIsj <- getDOI(citj[selfcitj])
          
          # Check only for DOI:
          # Extract DOIs:
          eqCitDOI <- outer(DOIsi,
                            DOIsj,
                            '==')
          
          eqCitDOI[is.na(eqCitDOI)] <- FALSE
          eqCitCit[is.na(eqCitCit)] <- FALSE
          # benchFinish("cit")
          if (all(dim(eqCitCit) > 0) && all(dim(eqCitCit) == dim(eqCitDOI)) && sum(eqCitCit | eqCitDOI) > 0 )
          {
            eqMat[i,j] <- eqMat[j,i] <- TRUE 
            # benchFinish("cit")
            next
          } 
        }
        
        setTxtProgressBar(pb, COMB)
    }
    close(pb)
    #         }
    #     }
    
    # benchStart("Communities")
    # Detect communities:
    iG <- graph.adjacency(eqMat, mode = "undirected")
    com <- fastgreedy.community(iG)
    
    # Update x:
    
    x$authorID <- ave(seq_len(nrow(x)),com$membership,FUN=min)
    x$curname <- ave(x$curname,x$authorID,FUN=function(x){
      paste(unique(x), collapse = " / ")
    })
    
    # benchFinish("Communities")
    # benchFinish("entire")
    
    if (all(xOld$authorID == x$authorID) | it >= maxIter)
    {
      break
    }
    
    
    it <- it + 1
    
    
    
  }
  
  if (plotGraph)
  {
    library(qgraph)
    library(igraph)
    
    coms <- paste("Author",x$authorID) 
    
    edgeList <- which(eqMat, arr.ind = TRUE)
    edgeList <- edgeList[edgeList[,1] > edgeList[,2],]
    
    Graph <- qgraph(edgeList, repulsion = 0.8, DoNotPlot = TRUE, groups = coms, legend = FALSE, esize = 1,
                    nNodes = nrow(x), edgelist = TRUE, directed = FALSE)
    
    qgraphAnnotate(Graph, 
                   Name = x$original_fullName, 
                   Authors =  x$AF,
                   Year =  x$PY,
                   Title =  gsub("[\'\"]","",x$TI),
                   authorID = x$authorID,
                   Address = gsub("[\'\"]","",x$address), fromqgraph = FALSE, filename = filename, image.size = "1200x1200")  
    
    
  }
  
  if (returnList)
  {
    return(list(
      data = x,
      equalmat = eqMat))
  }
  
  return(x)
}

# plot(foo, repulsion = 0.8)
