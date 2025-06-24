#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Download eosGP
# Coder: James Maze
# Date: 13 Jan 2021
# Purpose: Read the raw eosGP files as .dat
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


download_fun <- function(file_path){
  #Read the files
  temp <- read_delim(paste0(file_path),
                    delim = ",",
                    col_names = TRUE,
                    skip = 1) %>% 
    as_tibble() %>% 
    mutate(file = str_sub(file_path, 10)) %>% 
    mutate(Site_ID = str_sub(file, 1, 2))
  temp
}
