# Computes graph based of string of WOK output files:
wokData <- function(
  files, # List of WOK files
  IDby = "cleaned_fullName", # What variable to ID by?
  range=c(-Inf,Inf), # Year range
  fullName = TRUE, # If TRUE, use AF (Author Full name)
  address = TRUE, # If TRUE, use address
  firstInit  = FALSE, # If TRUE, use only first initial. Only if fullName == FALSE
#   weighted=FALSE, # Should the graph be weighted?
  ... # Not used
  )
{
  Tables <- do.call("rbind",lapply(files,readfun,range=range))
  
  if (nrow(Tables)==0) return(NULL)
  
  # Remove duplicates:
  Tables <- Tables[!duplicated(Tables),]
  
  # Extract data:
#   if (fullName) 
#   {
#     auth <- as.character(Tables$AF)
#   } else {
#     auth <- as.character(Tables$AU)
#   }
  year <- as.numeric(Tables$PY)
#   journal <- Tables$SO
  years <- as.numeric(sort(unique(year)))
  
  # Remove years not in range:
  Tables <- Tables[years >= range[1] & years <= range[2],]
  

  ## COMPUTE DATA FRAME:
  # For each author:
  # name, fullName, address, article, id (same is same author)
  Tables$articleID <- seq_len(nrow(Tables))
  
  fullData <- ddply(Tables,.(articleID),function(x){
    
    original_name <- unlist(strsplit(x$AU, split="; "))
    original_fullName <- unlist(strsplit(x$AF, split="; "))
    
    # If only one author:
    if (length(original_fullName) == 1)
    {
      original_address <- x$C1
    } else {
      # Split on character before [:
      addresses <- unlist(strsplit(x$C1, split=".(?=\\[)", perl=TRUE))
      # Obtain all addresses per author:
      original_address <- sapply(original_fullName, function(x)paste(addresses[grepl(paste0("^\\[.*?",x,".*?\\]"), addresses)], collapse = " + "))  
    }
    
      
    # Clean:
    cleaned_name <- strClean(original_name)
    cleaned_fullName <- strClean(original_fullName)
    
    
    # Initial:
    cleaned_nameFirstInit <-  gsub("(?<=\\,\\w).*","",cleaned_name,perl=TRUE)
    
    # Clean address:
    # Remove author:
    cleaned_address <- gsub("\\[.*?\\] ","",original_address)
    # Remove addres after first comma (only keep first university):
    cleaned_address <- gsub("\\,.*","",cleaned_address)
    # Clean:
    cleaned_address <- strClean(cleaned_address)
    
    # Combine cleaned fullname and address:
    fullName_address <- paste0(cleaned_fullName,".",cleaned_address)
    

    # Put in data frame:
    Res <- data.frame(
      original_name = original_name,
      original_fullName = original_fullName,
      cleaned_name = cleaned_name,
      cleaned_nameFirstInit = cleaned_nameFirstInit,
      cleaned_fullName = cleaned_fullName,
      cleaned_address = cleaned_address,
      fullName_address = fullName_address,
      articleID = x$articleID
      )
    
    
    return(Res)
  })
  
  # Id:
  fullData$ID <- as.numeric(as.factor(fullData[[IDby]]))
#   
#   
#   # Make author list:
#   authList <- authOrig <- strsplit(auth,split="\\;|\\.\\,|\\&|\\<and\\>")
#   authList <- lapply(authList,tolower)    
#   authList <- lapply(authList,gsub,pattern="[[:space:]]",replacement="")
#   authList <- lapply(authList,function(x)x[x!=""])
#   authList <- lapply(authList,gsub,pattern="[[:punct:]]|[[:space:]]",replacement="")
#   
#   ## CLEANUP RULES ##
#   ## FIRST INITIAL ONLY ###
#   if (firstInit) {
#     if (fullName) warning("firstInit ignored when full name is used") else authList <- lapply(authList,gsub,pattern="(?<=\\,\\w).*",replacement="",perl=TRUE)
#   }
#   
#   
#   
#   authUn <- data.frame(
#     code = unique(unlist(authList)),  
#     stringsAsFactors=FALSE)
#   
#   authUn$surname <- gsub(",(?<=\\,).*","",authUn$code,perl=TRUE)
#   
  #     x <- authUn$surname
  #     
  #     dist <- adist(x)
  #   
  #     close <- which(dist == 1,arr.ind=TRUE)
  #     for (i in 1:nrow(close))
  #     {
  #       print(paste(x[close[i,1]],x[close[i,2]]))
  #     }
  #     
  #     authUn <- data.frame(
  #       lowercase = unique(unlist(authList)),
  #       orig = NA)
  #     
  #     authUn$orig <- sapply(authUn$lowercase,function(x)authOrig[which(authOrig2==x)[1]])
  #     
  #     dotfun <- function(x)
  #     {
  #       if (x[length(x)]==" ") y <- x[-length(x)] else if (x[length(x)]==".") y <- x else y <- c(x,".")
  #       if (y[1]==" ") y <- y[-1]
  #       return(paste(y,collapse=""))
  #     }
  #     authUn$orig <- sapply(strsplit(authUn$orig,split=""),dotfun)
  
  # Compute Adjacency:
#   AuthxPub <- laply(authList,function(x)authUn$code%in%x,.drop=FALSE)
#   authAdj <- 1*(t(AuthxPub) %*% AuthxPub)
#   rm(AuthxPub)
#   
#   if (!weighted) authAdj <- 1*(authAdj>0)
#   
#   rownames(authAdj) <- colnames(authAdj) <- authUn$code
#   
#   Res <- list(
#     Adjacency = authAdj,
#     Names = authUn,
#     Raw = Tables
#   )
  
  class(fullData) <- c("authGraph", "data.frame")
  return(fullData)
  #   }
}
