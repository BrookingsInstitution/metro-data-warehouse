# functions created to write this package

# read excel files from url
readxl_online <- function(url, type = NULL, ...) {
  test <- stringr::str_detect(url, "[.]xls|[.]zip")
  if (test == FALSE) {
    print(message("Expecting file extension of type .xlsx, .xls or .zip. Check the URL or the data source for the correct file extension and use the type argument"))
  }
  # test for individual file extensions for xls use look forward, xls not
  # followed by x
  t1 <- stringr::str_detect(url, "[.]xlsx")
  t2 <- stringr::str_detect(url, "[.]xls(?!x)")
  tz <- stringr::str_detect(url, "[.]zip")
  if (t1 == TRUE) {
    type = ".xlsx"
  }
  if (t2 == TRUE) {
    type = ".xls"
  }
  if (tz == TRUE) {
    httr::GET(url, httr::write_disk("tmp.zip", overwrite = TRUE))
    tmp <- unzip("tmp.zip")
    # On osx more than one file name is returned, select first element.
    df <- readxl::read_excel(tmp[[1]],...)
    return(df)
  }
  if (!is.null(type)) {
    type = type

  }
  df <- httr::GET(url, httr::write_disk(paste0("tmp", type), overwrite = TRUE))
  df <- readxl::read_excel(paste0("tmp", type),...)

}


save_output <- function(df,labels,folder, file, title, contact, source){

  # create folder
  if (!dir.exists(folder)){dir.create(folder)}

  # save datasets

  save(df, file = paste0(folder,"\\/",file,".rda"))
  write_csv(df,paste0(folder,"\\/",file,".csv"))

  # generate metadata
  sink(paste0(folder,"\\/",file,".txt"),append = F)
  cat("Title: ",title)
  cat("\nContact: ", contact)
  cat("\nSource: ", source)
  cat("\nLast updated: ", date(), "\n\n")

  print(labels%>%kable())
  cat("\n\n")

  skimr::skim_with(numeric = list(hist = NULL), integer = list(hist = NULL))
  skimr::skim(df)%>%kable()

  sink()

  # create README
  sink(paste0(folder,"\\/README.md"),append = F)

  cat("Title: ",title)
  cat("\nContact: ", contact)
  cat("\nSource: ", source)
  cat("\nLast updated: ", date(), "\n\n")

  print(labels%>%kable())
  cat("\n\n")

  skimr::skim(df)%>% kable()
  sink()

}

