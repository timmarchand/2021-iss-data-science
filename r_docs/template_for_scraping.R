# load libraries ----------------------------------------------------------
pacman::p_load(tidyverse,rvest,clipr,datapasta)

# Read in and select data ------------------------------------------------------------
## Copy and paste method using scraper extension ----

## (1) Use clipr package
## Advantages - quick and easy
## Disadvantages - not reproducible; might accidentally paste the wrong data
## create a dataframe from whatever is copied  on your clipboard
dat <- clipr::read_clip_tbl()
## save data immediately
write_csv(dat,"data/scraped_data_name.csv")


## (2) Use datapasta ----
## Advantages - you can see and keep the data stored in your code file
## Disadvantages - not reproducible; can result in many lines of code

## Go to Addins
## Choose Paste as Tribble for dataframe
dat <-

## Choose Paste as Vector for single columns
col1 <-

## Choose Paste as Vector for single columns
col2 <-

## Coding method using rvest ----
##Get CSS with selector gadget and use rvest
## Advantages - clear steps, reproducible, easy to extend
## Disadvantages - requires some coding!

# assign url and CSS values - don't forget to use ""
url <-
CSS1 <-
CSS2 <-

# get html data
html <- read_html(url)
# select node from CSS value and read text
vec1 <- html_nodes(html, CSS1) %>% html_text
vec2<- html_nodes(html, CSS2) %>% html_text

dat <- tibble(col1 = vec1,
              col2 = vec2)

### Alternative: One pipe chain

dat <- tibble(col1 = read_html(url) %>%
                        html_nodes(CSS1) %>%
                        html_text(),
              col2 =  read_html(url) %>%
                        html_nodes(CSS2) %>%
                        html_text())

# Check and Wrangle data

--------------------------------------------------------------








# Visualise data ----------------------------------------------------------


