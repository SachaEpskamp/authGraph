
RefCheck <- function(acc=0.1)
    {
    
    refCheck <- function(x,mat=NULL,max.distance=0.2)
    {
    allUn <- unique(unlist(x))
    tab <- table(unlist(x))
    sim <- lapply(allUn, agrep, allUn, value = TRUE, max.distance=max.distance)
    Nsim <- sapply(sim,length)
    
    for (i in which(Nsim > 1))
    {
        cat("\nPossible duplicates found: \n\t ",sim[[i]],"\n")
    	if (!is.null(mat))
    	{
    		cat("Versions of original names: \n\t ",mat[as.character(mat[,1])%in%sim[[i]],2],"\n")
    	}
    	allMatch <- sapply(x,function(y)any(sim[[i]]%in%y))
    	cat("Matches found in references: \n\t",which(allMatch),"\n")
    	lowMatch <- sapply(x,function(y)any(names(which.min(tab[sim[[i]]]))%in%y))
    	cat("Matches of least frequent version found in references: \n\t",which(lowMatch),"\n")
    }
    }
    
    # Read refs:
    refs <- read.csv(file.choose(),sep="\t",header=FALSE)
    
    auth <- as.character(refs[,1])
    #year <- as.numeric(refs[,3])
    #journal <- refs[,2]
    #years <- as.character(sort(unique(year)))
    
    # Make author list:
    authList <- authOrig <- strsplit(auth,split="\\;|\\.\\,|\\&|\\<and\\>")
    authList <- lapply(authList,tolower)
    authList <- authOrig2 <- lapply(authList,gsub,pattern="[[:punct:]]|[[:space:]]",replacement="")
    authList <- lapply(authList,function(x)x[x!=""])
    
    authOrig <- unlist(authOrig)
    authOrig2 <- unlist(authOrig2)
    
    authUn <- data.frame(
        lowercase = unique(unlist(authList)),
    	orig = NA)
    
    authUn$orig <- sapply(authUn$lowercase,function(x)authOrig[which(authOrig2==x)[1]])
    
    dotfun <- function(x)
    {
    	if (x[length(x)]==" ") y <- x[-length(x)] else if (x[length(x)]==".") y <- x else y <- c(x,".")
    	if (y[1]==" ") y <- y[-1]
    	return(paste(y,collapse=""))
    }
    authUn$orig <- sapply(strsplit(authUn$orig,split=""),dotfun)
    
    refCheck(authList,authUn,acc)
}
