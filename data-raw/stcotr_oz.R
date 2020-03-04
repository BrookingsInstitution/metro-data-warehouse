# opportunity zones
oz_path <- "https://www.cdfifund.gov/Documents/Designated%20QOZs.12.14.18.xlsx"

downl_excel <- function(url, skip){
  temp = tempfile(fileext = ".xlsx")
  download.file(url, destfile=temp, mode='wb')
  readxl::read_excel(temp, skip = skip)
}

stcotr_oz <- downl_excel(oz_path, 4)

names(stcotr_oz) <- c("st_name", "co_name", "stcotr_code", "stco_type", "year_source")

usethis::use_data(stcotr_oz)

