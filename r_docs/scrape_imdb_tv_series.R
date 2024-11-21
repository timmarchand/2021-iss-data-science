scrape_imdb_tv <- function(tt, season = 1){
  # Load required libraries
  library(httr)
  library(rvest)
  library(tidyverse)
  library(glue)
  library(lubridate)
  library(snakecase)
  
  base_url <- glue("https://www.imdb.com/title/{tt}/")
  
  response <- GET(base_url, add_headers(
    .headers = c('user_agent' = 'Mozilla/5.0',
                 'accept_language' = 'en-US,en;q=0.9')
  ))
  
   show_name <- read_html(response) %>% 
     html_nodes("span.hero__primary-text") %>% 
     html_text(trim = TRUE) %>% 
     to_upper_camel_case()
  
if(is.character(season)){
  
  last_season <- read_html(response) %>% 
    html_nodes("label.ipc-simple-select__label") %>% 
    html_text %>% 
    parse_number %>% 
    first %>% 
    coalesce(1)
  
  season <- 1:last_season
  
}

grab_imdb <-
  function(tt, season) {

    
    url <- glue("https://www.imdb.com/title/{tt}/episodes?season={season}")
    
    # Grab the HTML data
    response <- GET(url, add_headers(
      .headers = c('user_agent' = 'Mozilla/5.0',
                   'accept_language' = 'en-US,en;q=0.9')
    ))
    raw_html <- read_html(response)
    
    # Select the specific nodes for episode info
    raw_div <- raw_html %>%
      html_nodes("div.sc-f2169d65-4.kBjDMi")
    
    # Helper function to extract text or return an NA
    extract_or_NA <- function(node, css) {
      
      result <- node %>%
        html_nodes(css) %>%
        html_text(trim = TRUE)
      
      if(length(result) < 1){result <- NA_character_}
      return(result)
    }
    # Get the title
    title <- map_chr(raw_div, ~extract_or_NA(.x, "div.ipc-title__text"))
    air_date <- map_chr(raw_div, ~extract_or_NA(.x, "span.sc-f2169d65-10.bYaARM"))
    rating <- map_chr(raw_div, ~extract_or_NA(.x, "span.ipc-rating-star--rating"))
    votes <- map_chr(raw_div, ~extract_or_NA(.x, "span.ipc-rating-star--voteCount"))
    plot <- map_chr(raw_div, ~extract_or_NA(.x, "div.ipc-html-content-inner-div"))
    
    convert_K <- function(x){str_replace(x, "K", "e3")}
    
    # Put them together in a tibble
    df <- tibble(show_name, season, title, air_date, rating, votes, plot) %>%
      mutate(air_date = suppressWarnings(mdy(air_date))) %>%
      mutate(votes = str_remove_all(votes, "[\\(\\)]") %>% convert_K) %>%
      mutate(episode = str_extract(title, "(?<=E)\\d+"),
             title = str_extract(title, "(?<= ∙ ).+$"), .after = season)
    
    
    # Let readr guess the variable types
    df <- suppressMessages(type_convert(df))
    
    return(df)
    
  }

map(season, ~grab_imdb(tt, season = .x)) %>%
    bind_rows()
}

save_imdb_tv <- function(tt,season = 1) {
  # Start scraping reviews for the given IMDb ID
  data <- scrape_imdb_tv(tt, season)
  show_name <- unique(data$show_name)
  # Create a directory named "reviews" if it does not exist
  dir.create("imdb_tv_episodes", showWarnings = FALSE)
  
  # Write the scraped data to a CSV file in the "reviews" directory
  write_csv(data, glue("imdb_tv_episodes/{show_name}_{tt}.csv"))
}


scrape_imdb_tv("tt0078703", "all")
save_imdb_tv("tt0108778", 1)
