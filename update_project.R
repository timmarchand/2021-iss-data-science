# setup ----
library(tidyverse)
update_folder <- function(week = 1:14,type = c("r_docs","PS","data","figs","R","Rmd"),...){
  read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRimSFdlfx8dNYlqvLHEwdipABwqWSaM7Si2iYgV7wKEDnIo2-f3LJiqBWTzaZ-MuU8cY-3zus6-dEH/pub?gid=1828289266&single=true&output=csv", col_types = cols()) %>% 
    filter(week %in% {{week}}, type %in% {{type}}) %>% 
    mutate(path = case_when(type == "figs" ~ paste0("figures/",name),
                            type == "data" ~ paste0("data/",name),
                            type == "R" ~ paste0("r_docs/", name),
                            type == "Rmd" ~ paste0("r_docs/", name),
                            type == "r_docs" ~ paste0("r_docs/", name),
                            type == "PS" ~ paste0("prob_sets/", name),
                            type == "base" ~ name,
                            TRUE ~ "ignore")) %>%
    filter(path != "ignore") %>% 
    select(url,path) %>% as.list %>% 
    pwalk(.f = function(url,path){download.file(url,path)})
}
# Instructions ----
## select the week you want to download the data for, and the type of data:
## "figs" for figures and images
## "r_docs" for R scripts and R Markdown
## "data" for csv and other data files
## "PS" for problem sets

## leave blank to download all available (this will take longer)


## e.g. update_folder(week = 2) --> for all types from week 2; 
## e.g. update_folder(week = 1:3, "r_docs") --> for first 3 weeks r_docs only
## e.g. update_folder() --> all available types for all available weeks
