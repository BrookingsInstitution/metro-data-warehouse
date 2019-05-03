
get_naics <- function(url){
  tmp <- readxl_online(url)%>%
    # remove columns contain only nas
    select_if(~sum(!is.na(.)) > 0)%>%
    select(-1)

  names(tmp) <- c("code.naics","name.naics")
  return(tmp)
}


get_naics_17_12<- function(url){
  tmp <- readxl_online(url, skip = 2,col_type = 'text')%>%
    # remove columns contain only nas
    select_if(~sum(!is.na(.)) > 0)

  names(tmp) <- c("code.naics6.2017","name.naics6.2017","code.naics6.2012","name.naics6.2012")
  return(tmp)
}

get_clustermapping <- function(url){
  tmp <- readxl_online(url,sheet = "NAICS")
  names(tmp) <- c("code.naics6.all","code.naics6.2012","code.naics6.2007","code.naics6.2002","code.naics6.1997",
                  "name.naics6.all", "traded.naics6",
                  "code.cluster.naics6","name.cluster.naics6","code.subcluster.naics6","name.subcluster.naics6")
  return(tmp)

}
