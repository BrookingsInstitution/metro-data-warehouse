path = "data"

# load all .rda to environment
temp = list.files(pattern = ".rda", recursive = T,path,full.names = T)

list(lapply(temp, load, .GlobalEnv))
rm(acs_var,temp)
names <- objects()
dfs <- mget(names)

# function to save environment data to csv
save_datasets <- function(..., folder, file) {
  # create folder
  if (!dir.exists(folder)) {
    dir.create(folder)
  }

  # save datasets in .rda and .csv
  x <- list(...)
  names(x) <- file
  save(list = names(x), file = paste0(folder, "\\/", file, ".rda"), envir = list2env(x))

  write_csv(..., paste0(folder, "\\/", file, ".csv"))
}

# run
purrr::map2(.x = dfs, .y = names, function(x,y)save_datasets(x,folder  = path,file = y))
