# Read .rda and export to .csv

export2csv <- function(file){
  load(file)
  ob.name <- sub(".*\\/(.+)\\..+","\\1",file) # not sure if I did this right, need more testing
  write.csv(mget(ob.name),
            file = gsub(".Rda",".csv",file))
}

# test
export2csv("data/county2bea.Rda")

# NOT WORKING, FIX LATER
export2csv_all <- function(folder){
  filepaths <- list.files(folder,full.names = T)
  files <- filepaths[grep(".rda", filepaths)]

  # purrr::map(files,export2csv)
  # return(files)
}

# DON"T RUN
# export2csv_all("data")
