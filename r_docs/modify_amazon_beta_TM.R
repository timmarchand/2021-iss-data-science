## load libraries
library(tidyverse)
library(rvest)
library(glue)

## get see() function
source("r_docs/see_regex.R")
## get grab_amazon_beta() function
source("r_docs/grab_amazon_beta.R")

## change the ASIN number for your own choice!
ASIN <- "B08QWY2FCR"
## scrape a few pages to test things first
pages <- 1:5

df <- pages %>% 
  map_dfr(~grab_amazon_beta(ASIN,.x))

df

## Regex list ----

# "^\\d\\.0" >> string starting with any number followed by a period and zero (e.g "3.0")
# "^The\\sOne"
# "^@@@"
# (?<=@@@)(\\d+|One)
# @@@\\d+
# "stars$" 
# "\\S+"
# "([^\\d]+)(\\d.+)"
# "([^ ]+\\s[^ ]+\\s[^ ]+)"
# "^(\\w+\\s\\w+\\s\\w+)"
# "^([A-z]+\\s[A-z]+\\s[A-z]+).*$"
# "\\b[a-z]{2,}ly\\b"
# (.+?)(?=\\son)
# (?<=Reviewed in )(.+?)
# (?<=Reviewed in )(.+?)(?=\\son)
# \\d+\\s\\w+\\s\\d+$


### tasks:
## from the date column extract: country and date
## from the rating column extract: star rating and helpful count


## from the date column extract: country and date
qwe <- df %>% sample_n(20) %>% pull(date)
qwe

see("(?<=Reviewed in )(.+?)(?=\\son)", string = qwe)
see("\\d+\\s\\w+\\s\\d+$", string = qwe)

df %>% 
  mutate(country = str_extract(date,"(?<=Reviewed in )(.+?)(?=\\son)" )) %>% 
  mutate(date = str_extract(date, "\\d+\\s\\w+\\s\\d+$"))


## from the rating column extract: star rating and helpful count
asd <- df %>% sample_n(20) %>% pull(rating)
asd

see("(?<=@@@)(\\d+|One)", string = asd)

df %>% 
  mutate(helpful = str_extract(rating,"(?<=@@@)\\d+|One")) %>% 
  mutate(rating = parse_number(rating)) %>% 
  mutate(helpful = case_when(is.na(helpful) ~ 0L,
                             helpful == "One" ~ 1L,
                             TRUE ~ as.integer(helpful)))




## Alternative method using tidyr::extract() ----

## tidyr::extract(col = column ti extract from,
# into = names of new columns,
# regex = capture matching patterns between (),
# remove = FALSE to keep the original column)


df %>% 
  tidyr::extract(col = "rating",
                 into = "helpful", 
                 regex = "@@@(\\d+|One)", 
                 remove = FALSE) %>% 
  tidyr::extract(col = "date", 
                 into = c("country", "date"),
                 regex = "Reviewed in (.+) on (.+)") %>% 
  # convert the date
  mutate(date = lubridate::dmy(date)) %>% 
  # parse the number for the rating
  mutate(rating = parse_number(rating)) %>% 
  # deal with NA values and One in helpful
  mutate(helpful = case_when(is.na(helpful) ~ 0L,
                             helpful == "One" ~ 1L,
                             TRUE ~ as.integer(helpful)))




# update the function -----------------------------------------------------


grab_amazon <- function(ASIN, page){
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
  
  
  df <- tibble::tibble(product,title, date,rating, review)   %>% 
    tidyr::extract(col = "rating",
                   into = "helpful", 
                   regex = "@@@(\\d+|One)", 
                   remove = FALSE) %>% 
    tidyr::extract(col = "date", 
                   into = c("country", "date"),
                   regex = "Reviewed in (.+) on (.+)") %>% 
    # convert the date
    mutate(date = lubridate::dmy(date)) %>% 
    # parse the number for the rating
    mutate(rating = parse_number(rating)) %>% 
    # deal with NA values and One in helpful
    mutate(helpful = case_when(is.na(helpful) ~ 0L,
                               helpful == "One" ~ 1L,
                               TRUE ~ as.integer(helpful)))
}


## try again with the new function

pages %>% 
  map_dfr(~grab_amazon(ASIN, .x))