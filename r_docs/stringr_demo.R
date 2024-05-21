##LOAD libraries ----
library(tidyverse) # mainly for stringr functions
library(htmltools) # to view text in html window

#' If you don't have htmltools installed, what
#' should you do?

## Read in the raw data from a messy webscrape from the ISS website
# Raw text data
raw_text <- read_lines("data/messy_scrape.txt")

## 1 EXAMINE the raw_text----

print(raw_text)
html_print(raw_text)


## This is a bit of a mess, but can you answer these questions?

#' How many Professors are listed?
#' How did you find them?
#' How are the surnames listed?
#' How about forenames?
#' How many teachers are Social Science specialists?
#' How about English specialists?
#' Are all the faculty full Professors?
#' Between which "anchors" are fields of expertise listed?



## Here are some regex you can use in the following code

#' "\\d+" Any digit or digits
#' "\\w+" Any word character or characters
#' "\\s" Any whitespace character
#' ".+?" Anything multiple times (lazy match)
#' ".+" Anything multiple times (greedy match)
#' "^" Start of string  anchor
#' "$" End of string anchor
#' "(?<=Professor)" Look ahead anchor for Professor
#' "(?=\\sFields)" Look behind anchor for Fields
#' "(?=,)" Look behind anchor for a comma
#' "[A-Z']"+ any uppercase letter or apostrophe multiple times
#' "English|Social Science" match English or Social Science
#' "Professor|Associate Professor" match Professor or Associate Professor
#' .+?(?=\\d+|$) A lazy match until either a digit or end of the string
#' 
#' Test some of these out with str_view_all()

str_view_all(raw_text, "\\d+", html = TRUE)

#' Difficult to see because there are no linebreaks
#' Let's add them by adding a new line before each digit

raw_view <- str_replace_all(raw_text, "(\\d+)", "\n\\1")
raw_view

## View all digits
str_view_all(raw_view, "\\d+", html = TRUE)
## View matches for English or Social Science
str_view_all(raw_view, "English|Social Science", html = TRUE)
#' View word character matches between 
#' Professor start anchor and comma end anchor
str_view_all(raw_view, "(?<=Professor)\\w+(?=,)", html = TRUE)
#' Which Professor is missed?

## 2 EXTRACT FROM raw_text ----

#' Let's start extracting strings from raw_text

# extract all numbers ----
## str_extract_all returns a list
str_extract_all(raw_text, "\\d+") 

#' How do we know it's a list?
#' What happens if you use str_extract() instead?
#' What type of data is the result?
#' How can we change it to something numeric?

# extract all surnames ----
# hint all surnames are just all capital letters except one!
str_extract_all(raw_text, "[A-Z']+")
#' What's wrong with this result?
str_extract_all(raw_text, "[A-Z']{2,}")
#' What's wrong with this result?
#' BIG hint: try a lazy match between two anchors

# extract all fullnames ----
#' hint: try another lazy match between two anchors
str_extract_all(raw_text, "")
#' What happens if you do a greedy match instead?


# extract  position of each faculty member ----
#' hint: you want to extract either Professor or Associate Professor
str_extract_all(raw_text, "")

# extract the fields of expertise ----
str_extract_all(raw_text, "(?<=Specialization).+?(?=\\d+)")
#' What's wrong with this?
#' hint the look ahead anchor should be a digit OR end of the string
str_extract_all(raw_text, "")


## 3 SPLIT raw_text first ----
#' Sometimes it is easier to separate the text 
#' into a vector of strings first

#' The digits 1~18 seem to be natural starting points
#' to split the text

str_split(raw_text, "\\d")
#' What's wrong with this?

str_split(raw_text, "\\d+")
#' Remember str_split creates a list object
#' How long is it?

#' To grab the first (and only) element, we use [[1]]
#' so that now we only have a string vector
str_split(raw_text, "\\d+")[[1]]

#' This is exactly what the function str_split_1 does
str_split_1(raw_text, "\\d+")

#' Save the result as entries
entries <- str_split_1(raw_text, "\\d+")

#' We still have an empty string at the front to remove
entries <- entries[entries != ""]
length(entries)

#' Now certain extractions are easier

#' Extract surnames only ----
str_extract(entries, "(?<=Professor)[A-Z']+")
#' What happens if you use str_extract_all() instead?
#' Why?

#' Extract forenames only ----
#' hint - try any word characters anchored
#' between a comma and space, and a space and Fields
str_extract(entries, "")

#' Extract the fields of expertise  ----
#'  we just need to anchor between
#' Specialization and the end of each string
str_extract(entries, "")
#' What's wrong with this?

#' Find the Social Science experts  ----
#' hint extract wither Social Science or English
str_extract(entries, "")
#' What happens when we extract strings
#'  starting with Social Science instead?
str_extract(entries, "^Social Science")
#' What happens if you use str_detect instead?
str_detect(entries, "^Social Science")
#' What happens if you use str_subset instead?
str_subset(entries, "^Social Science")



## 4 CREATE a dataframe ----

#' We often want to work with dataframes in R
#' We can only do this when all vectors are the same length

ISS_prof_df <- 
  tibble(surname = str_extract(entries, "(?<=Professor)[A-Z']+"),
         forename = str_extract(entries, "(?<=,\\s)\\w+(?=\\sFields)"),
         position = str_extract(entries, "Professor|Associate Professor"),
         specialty = str_extract(entries, "Social Science|English"),
         fields = str_extract(entries, "(?<=Specialization).+$")
)
ISS_prof_df

## make the df "tidy"
#' How is it different?

ISS_prof_df %>% 
  mutate(fields = str_squish(fields)) %>% 
  separate_longer_delim(fields, ",")

