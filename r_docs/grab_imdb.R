library(rvest)
library(tidyverse)
## make the function
grab_imdb <- 
  function(code,season = 1){
    # define the base url
    url <- glue::glue("https://www.imdb.com/title/{code}/episodes?season={season}")
    
    # Grab the html data
    raw_html <- read_html(url)
    
    # Select the specific nodes for episode info
    raw_div <- raw_html %>% 
      html_nodes("div.list.detail.eplist") %>% 
      html_nodes("div.info")
    
    # Get the episode number (hidden as a meta node)
    ep_num <- raw_div %>% 
      html_nodes("meta") %>% 
      html_attr("content")
    
    # Get the title
    title <-  raw_div %>% 
      html_nodes("strong a") %>% 
      html_text()  %>% 
      str_squish()
    
    # Get the air_date
    air_date <- raw_div %>% 
      html_nodes("div.airdate") %>% 
      html_text() %>% 
      str_squish() %>% 
      # convert to date format
      lubridate::dmy()
    
    # Get the ratings
    rating <- raw_div %>% 
      html_nodes("div.ipl-rating-star.small > span.ipl-rating-star__rating") %>% 
      html_text()
    
    # Get the ratings count (votes)
    votes <- raw_div %>% 
      html_nodes("div.ipl-rating-star.small > span.ipl-rating-star__total-votes")%>% 
      html_text() %>% 
      str_remove_all("\\(|\\)|,")
    
    # Get the plot description 
    plot <- raw_div %>% 
      html_nodes("div.item_description") %>% 
      html_text() %>% 
      str_squish()
    
    # Put them together in a tibble
    df <- tibble(season, ep_num, title, air_date, rating, votes, plot)  
    
    # Let readr guess the variable types
    df <- suppressMessages(type_convert(df))
    
  }