library(tidyverse)
library(rvest) # for scraping the website
library(httr) # for downloading and saving from the website

# Define constants
base_url <- "https://www.e-stat.go.jp"
page_url <- str_glue("{base_url}/en/stat-search/files?page=1&query=Population%20by%20Sex%20Ratio%20and%20Households%20Household%20Members%20Type%20of%20Groups%20Japan*%20All%20Shi%20Gun%20prefecture*%20prefecture%20Shi*%20Ku*%20Mac&layout=dataset&metadata=1&data=1")

# Read and parse the HTML page
page <- read_html(page_url)

# Extract all relevant relative links from the correct class
download_links <- page %>%
  html_elements(".stat-download_icon_left") %>%
  html_attr("href") %>%
  discard(is.na) %>%
  str_c(base_url, .)

prefecture <- page %>% 
  html_elements(".stat-resource_list-detail-item:nth-child(2)") %>% 
  html_text() %>% 
  str_squish() %>% 
  str_extract("(?<=\\d{1,2})\\S+$") %>% 
  str_remove("\\d+")

links_tbl <- tibble(prefecture, download_links) %>% 
  drop_na(prefecture) %>%     # Skip NA rows
  mutate(filename = str_replace_all(prefecture, "-", "_") %>%  # Create clean filenames
           str_c(".csv"))

links_tbl #the table of links


## Use a purrr function to download iteratively
links_tbl %>%
  pwalk(function(prefecture, download_links, filename) {
    # Define path
    dir.create("estat_csvs", showWarnings = FALSE)
    path <- file.path("estat_csvs", filename)
    
    # Download and write file
    tryCatch({
      GET(download_links, write_disk(path, overwrite = TRUE))
      message("Downloaded: ", prefecture)
    }, error = function(e) {
      message("Failed: ", prefecture, " - ", e$message)
    })
  })

## folder of csvs should appear in your project