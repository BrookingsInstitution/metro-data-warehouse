# use skimr::skim_to_list() to write metadata file for data frames

write_metadata <- function(df,filename){

  sink(filename)
  cat("Author: ", Sys.info()[["user"]])
  cat("\nLast modified", date())

}


write_summary <- function(df,filename){
  sink(filename, append = T)
  cat("\n")
  cat("\n")

  print(skimr::skim(df))
  # skimr::skim(df)
  sink()
}

write_meta <- function(df,filename){
  write_metadata(df,filename)
  write_summary(df,filename)
}

# test
write_meta(df = county_cbsa_st,
           filename = "data-raw/county_cbsa_st.txt")

write_meta(df = county2bea,
           filename = "data-raw/county2bea.txt")
