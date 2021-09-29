## Update data and figures folders
## Load library
library(tidyverse)
library(tidylog)
## read in url links
data_urls <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRimSFdlfx8dNYlqvLHEwdipABwqWSaM7Si2iYgV7wKEDnIo2-f3LJiqBWTzaZ-MuU8cY-3zus6-dEH/pub?gid=0&single=true&output=csv")
figs_urls <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRimSFdlfx8dNYlqvLHEwdipABwqWSaM7Si2iYgV7wKEDnIo2-f3LJiqBWTzaZ-MuU8cY-3zus6-dEH/pub?gid=1168463596&single=true&output=csv")
  
## create functions
output_jpg <- function(names, urls){ 
  ## select path in folder system
  folder_path <- "../figures/"
  ## download images according to urls and stores in named paths  
    download.file(urls, paste0(folder_path, names, ".jpg"))
}
output_csv <- function(urls, data){ 
  ## download images according to urls and stores in named paths  
 read_csv(urls) %>%  set_names(data) %>%  
    write_csv(paste0("../data/", data, ".csv"))
}

## apply functions and update folders
figs_urls %>% pmap(output_jpg)

test <- figs_urls[1,]
download.file(test$urls, paste0)

%>% purrr::pwalk(output_jpg) 

list(data = figs_urls$figs,
     urls = figs_urls$urls) %>% 
  purrr::pwalk(output_jpg) 

data_urls %>% as.list %>% map(read_csv) 

list(data = data_urls$data,
     urls = data_urls$urls) %>% pmap(output_csv)

data_urls$urls[1:2] %>% map(read_csv) %>% 
  map(~write_csv(.x, file =paste0("/data/", data_urls$data[1:2], ".csv")))

%>% 
  purrr::pmap(output_csv)

file_names <- data_urls$data[1:2]
file_paths <- data_urls$urls[1:2]

data_urls[1:2,] %>%
  purrr::map(function(file_name){ # iterate through each file name
    read_csv(paste0(file_paths, file_names))
  }) -> df_list_read2 # Assign to a list

data_urls[1:2,] %>% pmap(output_csv)
