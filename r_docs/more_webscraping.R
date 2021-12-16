library(rvest)
library(tidyverse)



# map rvest functions on WHO links ----

### load data from WHO

who_news <- read_csv("data/who_news.csv")

## map read_html for each link
## this creates a new list column called html
who_html <- who_news %>% 
  mutate(html = map(url, ~read_html(.x)))
who_html

## IF THIS DOES NOT WORK, SOME OF THE LINKS MIGHT BE DEAD
## Re-create the data frame from the webscraping part II file!

## map html_nodes and html_text on the list column
## this creates a new list column called text
who_text <- 
  who_html %>% mutate( text = map(html, ~html_nodes(.x, css = ".sf-detail-body-wrapper div") %>% 
                                    html_text(trim = TRUE)))
who_text


## to see the contents of a nested list column, use unnest
who_text %>% 
  unnest(text)

## what has happened to the number of rows??

## put all the texts from the same headline together
## group_by headline, summarise and then paste with a collapse argument
who_text %>% 
  unnest(text) %>% 
  roup_by(headline, date) %>% 
  summarise(text = paste(text, collapse = " ")) 


## As above, in one pipe:

## map read_html for each link
who_texts <- 
  who_news %>% 
  mutate(html = map(url, ~read_html(.x)),
         ## grab text from each page
         text = map(html, ~html_nodes(.x, css = ".sf-detail-body-wrapper div") %>% 
                      html_text(trim = TRUE))) %>%
  ## unnest the list column
  unnest(text) %>%
  ## put all the texts together with paste and collapse
  group_by(headline, date) %>% 
  summarise(text = paste(text, collapse = " ")) 



# Case study - top 250 films from IMDB -------------------------------------


## This website list the top rated movies
url <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"

## Question: How would you find out about the directors of these movies?
## For example, which country were they born?
## How old were they when they made the top rated movie etc?

## Each movie has it's own link, so let's collect those details first, plus the titles, years and ratings



base <- "https://www.imdb.com" # to add to links

css1 <- ".titleColumn a" ## css for movie title
css2 <- ".secondaryInfo" ## css for year
css3 <- "strong" ## css for rating

## get the html data
html <- read_html(url)


# film_titles <- read_csv(film, file = "data/film_titles.csv")


year <- html %>% 
  html_nodes(css2) %>% 
  html_text() %>% parse_number


rating <- html %>% 
  html_nodes(css3) %>% 
  html_text() %>% parse_number

link <- html %>% 
  html_nodes(css1) %>% 
  html_attr("href") %>% 
  paste0(base,.)


film_tib <- tibble(rank = 1:250,
                   title,
                   year,
                   rating,
                   link)

## Use this in case the film titles are a bit weird!
# film_titles <- read_csv(film, file = "data/film_titles.csv")
# film_tib <- 
# film_tib %>% inner_join(film_titles,.)

### go to links for more info ----

## get html nodes for each film link:

 tib_html <- film_tib %>% 
   mutate(html = map(link,~.x %>% read_html))

## This code may take a long time, so go do something else while it runs!


## director details ---- 

## css
director <- '//div[3]/ul/li[1]/div/ul/li[1]/a'

tib_director <- tib_html %>% 
  transmute(rank, title, year, rating,
            director = map(html, ~.x %>% 
                             html_elements(xpath = '//div[3]/ul/li[1]/div/ul/li[1]/a') %>% 
                             html_text()),
            link = map(html, ~.x %>%
                         html_elements(xpath = '//div[3]/ul/li[1]/div/ul/li[1]/a') %>%
                         html_attr("href") %>% paste0(base,.))) %>%
  unnest(cols = c(director,link), keep_empty = TRUE)

  
  ## Now we can go to the director pages, and grab the necessary details from there

 director_html <- tib_director %>% 
                    mutate(html = map(link,~.x %>% read_html))

## This code may take a long time, so go do something else while it runs!



## css for bio details

borninfo <- "#name-born-info"
deathinfo <- "#name-death-info"

director_details <- director_html %>% 
  mutate(borninfo = map(html, ~ .x %>% 
                          html_elements(borninfo) %>% 
                          html_text() %>% str_squish),
         deathinfo = map(html, ~ .x %>% 
                           html_elements(deathinfo) %>% 
                           html_text() %>% str_squish)) %>% 
  ## We use the keep_empty argument to avoid NA rows being dropped
  unnest(cols = c(borninfo,deathinfo), keep_empty = TRUE)


life_details <- director_details %>% 
  select(rank, title, year, director, borninfo, deathinfo) %>% 
  # extract date and place of birth from borninfo col
  extract(col = borninfo, into = c("birthday", "birthplace"),regex = "(?<=Born:\\s)(.+?)\\sin\\s(.+?)$", remove = FALSE) %>% 
  # convert birthday to date format
  mutate(birthdate = lubridate::mdy(birthday)) %>% 
  # get birthyear (extract birthyear for those without birthday data)
  mutate(birthyear = ifelse(str_detect(borninfo, "Born:\\s\\d+"), birthday, lubridate::year(birthdate)) %>% as.double)  %>%  
  # get approx age at film release
  mutate(age_at_release = year - birthyear) %>% 
  ## extract country from birthplace
  mutate(country = str_extract(birthplace, "(?<=,)[^,]+$")) %>% 
  ## dealing with places located in new countries (find 'now')
  mutate(country = ifelse(str_detect(country,"now"), str_extract(country, "(?<=now\\s)(.*)"), country)) %>% 
  ## remove square brackets and trailing whitespaces
  mutate(country = str_remove(country,"\\]") %>% str_squish) %>% 
  mutate(country = ifelse(is.na(country), birthplace, country))



## get info on death if applicable

life_details <- life_details %>% 
  # extract date and place of death from deathinfo col
  extract(col = deathinfo, into = c("death_date", "age_died","death_place"),regex = "(?<=Died:\\s)([^\\()]+).+?age(.+?)\\sin\\s(.+?)$", remove = FALSE) %>% 
  # convert death_date to date format
  mutate(death_date = lubridate::mdy(death_date)) %>% 
  # get birthyear
  mutate(death_year = lubridate::year(death_date)) %>% 
  # get approx age
  mutate(released_before_passing = death_year - year) %>% 
  mutate(age_died = parse_number(age_died)) %>% 
  mutate(alive = is.na(death_date))



## example visualisation 
life_details %>% 
  drop_na() %>% 
  # pick top 8, lump the rest together as other
  mutate(country = fct_lump(country,8),
  # reorder by country count
         country = fct_infreq(country)) %>% 
  ggplot(aes(age_at_release, fill = country)) +
  geom_histogram() +
  facet_wrap(~country)

write_csv(life_details, "data/top_250_director_lives.csv")

## Film financials ----

## xpaths
## we need two sets because some film pages have more sections

budget1 <- "//section[12]/div[2]/ul/li[1]/div/ul/li[1]/span"
budget2 <- "//section[11]/div[2]/ul/li[1]/div/ul/li[1]/span"
us_gross1 <- "//section[12]/div[2]/ul/li[2]/div/ul/li[1]"
us_gross2 <- "//section[11]/div[2]/ul/li[2]/div/ul/li[1]"
opening1 <- "//section[12]/div[2]/ul/li[3]/div/ul/li[1]"
opening2 <- "//section[11]/div[2]/ul/li[3]/div/ul/li[1]"
world_gross1 <- "//section[12]/div[2]/ul/li[4]/div/ul/li[1]"
world_gross2 <- "//section[11]/div[2]/ul/li[4]/div/ul/li[1]"

opening <- "//section[11]/div[2]/ul/li[3]/div/ul/li[1]"


financials <- tib_html %>% transmute(rank, title, year, rating,
                                     budget1 = map(html, ~.x %>% 
                                                     html_elements(xpath = budget1) %>% 
                                                     html_text()),
                                     budget2 = map(html, ~.x %>% 
                                                     html_elements(xpath = budget2) %>% 
                                                     html_text()),
                                     opening1 = map(html, ~.x %>% 
                                                      html_elements(xpath = opening1) %>% 
                                                      html_text()),
                                     opening2 = map(html, ~.x %>% 
                                                      html_elements(xpath = opening2) %>% 
                                                      html_text()),
                                     us_gross1 = map(html, ~.x %>% 
                                                       html_elements(xpath = us_gross1) %>% 
                                                       html_text()),
                                     us_gross2 = map(html, ~.x %>% 
                                                       html_elements(xpath = us_gross2) %>% 
                                                       html_text()),
                                     world_gross1 =map(html, ~.x %>% 
                                                         html_elements(xpath = world_gross1) %>% 
                                                         html_text()),
                                     world_gross2 =map(html, ~.x %>% 
                                                         html_elements(xpath = world_gross2) %>% 
                                                         html_text()))



financials <- financials %>% 
  mutate(budget = ifelse(lengths(budget1) > 0, budget1,budget2),
         opening = ifelse(lengths(budget1) > 0, opening1,opening2),
         us_gross = ifelse(lengths(budget1) > 0, us_gross1,us_gross2),
         world_gross = ifelse(lengths(budget1) > 0, world_gross1,world_gross2))  %>% 
  unnest(cols = c(budget,opening,us_gross,world_gross), keep_empty = TRUE) %>% 
  select(rank,title, year,rank,budget,opening,us_gross,world_gross)

## have a look
financials




# film plot and review ----------------------------------------------------

## we need two sets of xpaths again due to the different page structures

plot1 <- '//*[@id="__next"]/main/div/section[1]/div/section/div/div[1]/section[7]/div[2]/div[1]/div[1]/div'
plot2 <- '//*[@id="__next"]/main/div/section[1]/div/section/div/div[1]/section[6]/div[2]/div[1]/div[1]/div'
review1 <- '//*[@id="__next"]/main/div/section[1]/div/section/div/div[1]/section[9]/div[2]/div[1]/div[3]/div[1]/div'
review2 <- '//*[@id="__next"]/main/div/section[1]/div/section/div/div[1]/section[8]/div[2]/div[1]/div[3]/div[1]/div'

plot_reviews <- 
  tib_html %>% mutate(
    plot1 = map(html,~.x %>% 
                  html_elements(xpath = plot1) %>% 
                  html_text()),
    plot2 = map(html,~.x %>% 
                  html_elements(xpath = plot2) %>% 
                  html_text()),
    review1 = map(html,~.x %>% 
                    html_elements(xpath = review1) %>% 
                    html_text()),
    review2 = map(html,~.x %>% 
                    html_elements(xpath = review2) %>% 
                    html_text())) %>% 
  mutate(plot = ifelse(lengths(plot1) > 0, plot1,plot2),
         review = ifelse(lengths(plot1) > 0, review1,review2)) %>% 
  unnest(cols = c(plot,review), keep_empty = TRUE) %>% 
  select(rank, title, year,rating, plot, review)

plot_reviews

## save all the data! ----

write_csv(who_texts, "data/who_texts.csv")
write_csv(life_details, "data/top_250_director_lives.csv")
write_csv(financials, "data/top_250_financials.csv")
write_csv(plot_reviews, "data/top_250_plot_reviews.csv")


## Challenges ----

## IMDB

## Filter to find movies that are in the list around a significant year for you
## (Birthday etc)

## IMDB directors

## Process the dataframes to find out and visualise
## Top ten countries by directors' birthplace
## Average age of the directors of the top rated film (is there a difference by country?)
## Any correlations between age, country and rating?


## IMDB financials

## Process the dataframe to convert to numerical data, then find:

## Most expensive films in the top 250
## Most profitable films in the top 250
## Highest ranked film considering budget
## Highest ranked film considering opening
## Highest ranked film considering gross

## IMDB plots and reviews


## Find out the most frequent words in the plots / reviews
## Which reviews share the most / least words from their plots
## Find out the most significant words in each review / plot


## WHO

## Find out which news document is the longest
## Find out the most frequent words in the news documents
## Find out the most significant words in the news documents





