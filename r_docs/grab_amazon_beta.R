## Load libraries ----
pacman::p_load(tidyverse, # of course
               rvest) # for scraping

## function
grab_amazon_beta <- function(ASIN, page){
  # define url, using glue for combining strings
  base_url <- "https://www.amazon.co.uk/product-reviews/"
  page_url <-  glue::glue("{base_url}{ASIN}/?pageNumber={page}")
  
  # go get the html
  html <- read_html(page_url)
  
  product <- html_nodes(html, ".a-text-ellipsis") %>% 
    html_text %>% stringi::stri_remove_empty() %>% str_squish()
  title<- html_nodes(html, ".a-text-bold span") %>% 
    html_text %>% stringi::stri_remove_empty()
  rating <- html_nodes(html, "#cm_cr-review_list .review-rating , .cr-vote-text") %>%
    html_text(trim = TRUE) %>% stringi::stri_remove_empty()
  ## pick out the strings starting with 3.0 etc
  starts <- str_detect(rating, "^\\d[.]0"); starts
  ## create a temp id variable of cumulative summing starts
  id <- cumsum(starts)
  ## split rating by id, merging the helpful variable
  rating <- sapply(split(rating, id), paste, collapse = "@@@") %>% as.character()
  
  date <- html_nodes(html, "#cm_cr-review_list .review-date") %>%
    html_text(trim = TRUE) %>% stringi::stri_remove_empty()
  
  review <- html_nodes(html, ".a-spacing-small.review-data") %>% 
    html_text(trim = TRUE) %>% stringi::stri_remove_empty()
  
  # make a tibble from each page 
  
  
  df <- tibble::tibble(product,title, date,rating, review) 
  
}
