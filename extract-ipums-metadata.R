
library(stringr)
library(readr)
library(tibble)
library(dplyr)

terra_pop_info <- readr::read_file("./data/ipums/raw/terrapop/terrapop_extract_15086.txt")

extract_meta_data <- function(metadata, header_line, seperator){
  
  header_line <- paste0(header_line, ":\n")
  
  
  meta_data_start <- stringr::str_locate_all(metadata,header_line)[[1]] %>% tibble::as_tibble()
  meta_data_end <- stringr::str_locate_all(metadata,"\n\n") [[1]] %>% tibble::as_tibble()
  meta_data_end <- min(meta_data_end$start[meta_data_end$start >meta_data_start$end])
  
  result <- str_sub(terra_pop_info, start=meta_data_start$end, end=meta_data_end)
  result <- str_trim(result)
  result <- unlist(strsplit(result, "\n"))
  
  result <- lapply(result, function(row){
    
    # if (header_line == "Area-level Variables")  
    # {
      mod_row <- unlist(strsplit(row, seperator))
    # }
    # 
    # if (header_line=="Raster Variables")
    # {
    #   mod_row <- unlist(strsplit(row, "\t"))
    # }
    # 
    variable <- mod_row[1]
    definition <- paste0(mod_row[2:length(mod_row)], collapse=" ")
    
    row_result <- tibble::as_tibble(list(
      variable=variable,
      definition=definition
    ))
    return(row_result)
    
  }) %>% dplyr::bind_rows()
  
  return(result)
  
}


pop_data <- extract_meta_data(terra_pop_info,"Area-level Variables", " ")
raster_data <- extract_meta_data(terra_pop_info,"Raster Variables","\t")

write_csv(pop_data, "./data/pop_variables.csv")
write_csv(raster_data, "./data/raster_variables.csv")


