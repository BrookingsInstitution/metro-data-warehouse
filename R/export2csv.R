# Read .rda and export to .csv

export2csv <- function(file){
  ob.name <- load(file)
  write.csv(get(df),file = gsub(".rda",".csv",file))
}

# TEST RUN
# export2csv("data/county2bea.rda")

# Export all --------------------------------------
export2csv_all <- function(folder){
  filepaths <- list.files(folder,full.names = T)
  files <- filepaths[grep(".rda", filepaths)]

  purrr::map(files,export2csv)
}

# TEST RUN
# export2csv_all("data")
