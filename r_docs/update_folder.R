# source script for update_folder function

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
    filter(include) %>% 
    select(url,path) %>% as.list %>% 
    pwalk(.f = function(url,path){download.file(url,path)})
}
