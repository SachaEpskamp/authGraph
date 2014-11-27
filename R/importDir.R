readfun <- function(x,range)
{
#   cols <- c("PT",  "AU",	"BA",	"BE",	"GP",	"AF",	"BF",	"CA",	"TI",	"SO",	"SE",	"LA",	"DT",	"CT",	"CY",	"CL",	"SP",	"HO",	"DE",	"ID",	"AB",	"C1",	"RP",	"EM",	"RI",	"FU",	"FX",	"CR",	"NR",	"TC",	"Z9",	"PU",	"PI",	"PA",	"SN",	"BN",	"J9",	"JI",	"PD",	"PY",	"VL",	"IS",	"PN",	"SU",	"SI",	"MA",	"BP",	"EP",	"AR",	"DI",	"D2",	"PG",	"P2",	"WC",	"SC",	"GA",	"UT")
  Tables <- read.table(x,header=TRUE,sep="\t",fileEncoding="UTF-16",fill=TRUE,stringsAsFactor=FALSE,quote=NULL, row.names=NULL)
  names(Tables) <- c(names(Tables)[names(Tables)!="row.names"],rep(NA,ncol(Tables)-sum(names(Tables)!="row.names")))

  # Remove duplicates:
  Tables <- Tables[!duplicated(Tables),]

  # Select only relevant columns:
#    Tables <- Tables %.% select(c(AU, AF, PD, PY, DT, DE, ID, EM, CR, TC, SO, TC, C1, UT, SC))
  # Keep all columns!
  Tables$articleID <- seq_len(nrow(Tables))
  
  # Extract data:
#   auth <- as.character(Tables$AU)
  year <- as.numeric(Tables$PY)
#   journal <- Tables$SO
  years <- as.character(sort(unique(year)))

  # Remove years not in range:
  Tables <- Tables %.% filter(year >= range[1] & year <= range[2])

  return(Tables)
}

importDir <- function(dir,format="wok", recursive = FALSE, ...)
{
  if (missing(dir)) dir <- tk_choose.dir()
#   if (format!="wok")
#   {
    # Import data:

    txtfiles <- list.files(dir,pattern="\\.txt",full.names=TRUE, recursive = recursive)
    if (any(grepl("niet",txtfiles,ignore.case=TRUE))) return(NULL)

    if (format!="wok") stop("Only Web of Knowledge format supported") else {
      return(wokData(files=txtfiles,...))
    }

#     Tables <- do.call("rbind.fill",lapply(txtfiles,readfun,range=range))
#     Tables <- do.call("rbind",lapply(txtfiles,readfun,range=range))
#   
#     if (nrow(Tables)==0) return(NULL)
# 
#     # Remove duplicates:
#     Tables <- Tables[!duplicated(Tables),]
#     
#     # Extract data:
#     if (fullName) 
#     {
#       auth <- as.character(Tables$AF)
#     } else {
#       auth <- as.character(Tables$AU)
#     }
#     year <- as.numeric(Tables$PY)
#     journal <- Tables$SO
#     years <- as.character(sort(unique(year)))
#     
#     # Remove years not in range:
#     Tables <- Tables[years >= range[1] & years <= range[2],]
#     
#     # Make author list:
#     authList <- authOrig <- strsplit(auth,split="\\;|\\.\\,|\\&|\\<and\\>")
#     authList <- lapply(authList,tolower)    
#     authList <- lapply(authList,gsub,pattern="[[:space:]]",replacement="")
#     authList <- lapply(authList,function(x)x[x!=""])
#         authList <- lapply(authList,gsub,pattern="[[:punct:]]|[[:space:]]",replacement="")
#     
#     ## CLEANUP RULES ##
#     ## FIRST INITIAL ONLY ###
#     if (firstInit) {
#       if (fullName) warning("firstInit ignored when full name is used") else authList <- lapply(authList,gsub,pattern="(?<=\\,\\w).*",replacement="",perl=TRUE)
#     }
# 
#     authUn <- data.frame(
#       code = unique(unlist(authList)),  
#       stringsAsFactors=FALSE)
#     
#     authUn$surname <- gsub(",(?<=\\,).*","",authUn$code,perl=TRUE)
# 
# #     x <- authUn$surname
# #     
# #     dist <- adist(x)
# #   
# #     close <- which(dist == 1,arr.ind=TRUE)
# #     for (i in 1:nrow(close))
# #     {
# #       print(paste(x[close[i,1]],x[close[i,2]]))
# #     }
# #     
# #     authUn <- data.frame(
# #       lowercase = unique(unlist(authList)),
# #       orig = NA)
# #     
# #     authUn$orig <- sapply(authUn$lowercase,function(x)authOrig[which(authOrig2==x)[1]])
# #     
# #     dotfun <- function(x)
# #     {
# #       if (x[length(x)]==" ") y <- x[-length(x)] else if (x[length(x)]==".") y <- x else y <- c(x,".")
# #       if (y[1]==" ") y <- y[-1]
# #       return(paste(y,collapse=""))
# #     }
# #     authUn$orig <- sapply(strsplit(authUn$orig,split=""),dotfun)
#     
#     # Compute Adjacency:
#     AuthxPub <- laply(authList,function(x)authUn$code%in%x,.drop=FALSE)
#     authAdj <- 1*(t(AuthxPub) %*% AuthxPub)
#     rm(AuthxPub)
#   
#   if (!weighted) authAdj <- 1*(authAdj>0)
#     
#     rownames(authAdj) <- colnames(authAdj) <- authUn$code
#     
#     Res <- list(
#       Adjacency = authAdj,
#       Names = authUn,
#       Raw = Tables
#       )
#     
#     class(Res) <- "authGraph"
#     return(Res)
# #   }
}
