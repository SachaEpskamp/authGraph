# library('httr')
# library('XML')
# library("plyr")

# example: wokDataQuery("SO=psychometrika")
wokDataQuery <- function(query, startDate = '1900-01-01', endDate = as.character(Sys.Date() + 1))
{
  ### OPEN THE SESSION ###
  postresult <- POST("http://search.webofknowledge.com/esti/wokmws/ws/WOKMWSAuthenticate", body = '<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:auth="http://auth.cxf.wokmws.thomsonreuters.com">
  <soapenv:Header/>
  <soapenv:Body>
  	<auth:authenticate/>
	</soapenv:Body>
</soapenv:Envelope>' )
  
  result <- as.character(postresult)
  
  xml <- xmlTreeParse(result)
  root <- xmlRoot(xml)
  
  SID <- xmlValue(root[["Body"]][["authenticateResponse"]][["return"]][[1]])
  
  ### Get some result (first value first to get range):
  body <- paste0('
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">
 <soapenv:Header/>
   <soapenv:Body>
    <woksearchlite:search>
      <queryParameters>
        <databaseId>WOS</databaseId> 
        <userQuery>',query,'</userQuery>
       <timeSpan>
        <begin>',startDate,'</begin>
        <end>',endDate,'</end>
       </timeSpan> 
      <queryLanguage>en</queryLanguage>
    </queryParameters>
    <retrieveParameters>
      <firstRecord>1</firstRecord>
      <count>1</count>
    </retrieveParameters>
   </woksearchlite:search>
 </soapenv:Body>
</soapenv:Envelope>
')
  
#   
#   body <- paste0('
# <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
# xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">
#  <soapenv:Header/>
#    <soapenv:Body>
#     <woksearchlite:search>
#       <queryParameters>
#         <databaseId>WOS</databaseId> 
#         <userQuery>',query,'</userQuery>
#        <timeSpan>
#         <begin>',startDate,'</begin>
#         <end>',endDate,'</end>
#        </timeSpan> 
#       <queryLanguage>en</queryLanguage>
#     </queryParameters>
#     <retrieveParameters>
#       <firstRecord>1</firstRecord>
#       <count>1</count>
#     </retrieveParameters>
#    </woksearchlite:search>
#  </soapenv:Body>
# </soapenv:Envelope>
# ')
#   
#   
#   body <- '
#   <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" 
#  xmlns:woksearch="http://woksearch.v3.wokmws.thomsonreuters.com">
#   <soapenv:Header/>
#   <soapenv:Body>
#   <woksearch:retrieveById>
#   <databaseId>WOS</databaseId> 
#   
#   <uid>WOS:000270372400005</uid>
#   <uid>WOS:000075022300003</uid>
#   
#   <queryLanguage>en</queryLanguage>
#   
#   <retrieveParameters>
#   <firstRecord>1</firstRecord>
#   <count>2</count>
#   <sortField>
#   <name>AU</name>
#   <sort>A</sort>
#   </sortField> 
#   </retrieveParameters>
#   </woksearch:retrieveById>
#   </soapenv:Body>
#   </soapenv:Envelope>
#   '
#   
#   SID <- 'N1RjWy5mhQES8Le9VSW'
  
  postresult <- POST("http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite", body = body, add_headers(SID = SID))
#   postresult <- POST("http://search.webofknowledge.com/esti/wokmws/ws/WokSearch", body = body, add_headers(SID = SID))
  postresult
  
  result <- as.character(postresult)
  # xml <- xmlTreeParse(result)
  browser()
  # To list:
  xmlList <- xmlToList(result)
  queryId <- xmlList$Body$searchResponse$return$queryId
  totalFound <- as.numeric(xmlList$Body$searchResponse$return$recordsFound)
  
  if (length(totalFound)==0 || totalFound == 0) stop("No records found!")
  
  ## Retrieve more results:
  Sequence <- seq(0, totalFound, by = 100)
  if(Sequence[length(Sequence)] != totalFound) Sequence <- c(Sequence, totalFound)
  
  ResultsList <- list()
  for (i in 1:(length(Sequence)-1))
  {
    body <- sprintf('
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
<soap:Body>
 <ns2:retrieve xmlns:ns2="http://woksearchlite.v3.wokmws.thomsonreuters.com">
 
 <queryId>%s</queryId>
 
 <retrieveParameters>
 <firstRecord>%s</firstRecord>
 <count>%s</count>
 </retrieveParameters>
 
 </ns2:retrieve>
</soap:Body>
</soap:Envelope>
', queryId, Sequence[i]+1, Sequence[i+1] - Sequence[i])
    
    postresult <- POST("http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite", body = body, add_headers(SID = SID))
    result <- as.character(postresult)
    xmlList <- xmlToList(result)
    Results <- xmlList$Body$retrieveResponse$return[which(names(xmlList$Body$retrieveResponse$return) == "records")]
    
    ResultsList <- c(Results, ResultsList)
  }
  
  if (length(ResultsList) != totalFound) warning("Length of results does not match query length. Something went wrong!")
  
#   ref <- ResultsList[[10]]
#   part <- ref$authors
#   
  
  ResultsTable <- ldply(ResultsList, function(ref){ as.data.frame(lapply(ref,function(part){
    if (is.list(part))
    {
      res <- data.frame(paste(part[names(part)=="value"], collapse = "; "))
      names(res) <- part$label
    } else res <- part
    return(res)
  }), stringsAsFactors = FALSE)})
  
  # Replace some names with abbreviations:
  
  
  ### Close session:
  body <- '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
  <!-- the session ID is sent in the HTTP header and is not part of the soap body -->
  <WOKMWSAuthentcate:closeSession 
xmlns:WOKMWSAuthentcate="http://auth.cxf.wokmws.thomsonreuters.com"/>
  </soap:Body>
  </soap:Envelope>'
  
  POST("http://search.webofknowledge.com/esti/wokmws/ws/WOKMWSAuthenticate", body = body, add_headers(SID = SID))
  


  # Extract relevant data and rename:
  Res <- subset(ResultsTable, select = c("Title", "Authors", "SourceTitle","Published.BiblioYear"))
  names(Res) <- c("TI","AU","SO","PY")

  return(Res)
}