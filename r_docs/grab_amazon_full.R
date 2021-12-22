## Load libraries ----
pacman::p_load(tidyverse, # of course
               rvest, # for scraping
               franc, # for guessing languages
               sentimentr, # for sentiment analysis
               lubridate) # to handle dates

## function
  grab_amazon_full <- function(ASIN, page){
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
    stars <- str_extract(rating,"\\d") %>% as.integer()
    helpful <- str_extract(rating, "(?<=@@@)\\d+") %>% as.integer() %>% imputeTS::na_replace()
    review <- html_nodes(html, ".a-spacing-small.review-data") %>% 
      html_text(trim = TRUE) %>% stringi::stri_remove_empty()
    date <- html_nodes(html, "#cm_cr-review_list .review-date") %>%
      html_text(trim = TRUE) %>% stringi::stri_remove_empty()
    country <- date %>% str_extract("(?<=in\\s).+?(?=\\son)")
    date <- date %>% str_extract("(?<=on\\s).+?$") %>% dmy()
   
    
    # make a tibble from each page 
  

      df <- tibble::tibble(product,title, date,country,stars,helpful,review)
      
      
      if(nrow(df) == 0){
        return(df)
      }else{
        df <- df %>% 
          mutate(language = map_chr(review,franc::franc),
                 title_sent = map(title,sentiment_by),
                 review_sent = map(review, sentiment_by)) %>%
          ## pick out the sentiment values
          hoist(.col = title_sent,
                title_sentiment = "ave_sentiment",
                title_wc = "word_count") %>%
          hoist(review_sent,
                review_sentiment = "ave_sentiment",
                review_wc = "word_count") %>%
          select(-title_sent, - review_sent)
        
        return(df)
      }
  }
  