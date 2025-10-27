##LOAD libraries ----
library(tidyverse) # mainly for stringr functions
library(htmltools) # to view text in html window

#' If you don't have htmltools installed, what
#' should you do?

## Read in the raw data from a messy webscrape from the ISS website
# Raw text data
raw_text <- read_file("data/iss_scrape.txt")
raw_lines <- read_lines("data/iss_scrape.txt")
## 1 EXAMINE the raw_text----

# What's the difference?
print(raw_text)
print(raw_lines)

# What's the difference?
length(raw_text)
length(raw_lines)

# What's the difference?
str_length(raw_text)
str_length(raw_lines)

# What's the difference?
html_print(raw_text)
html_print(raw_lines)

#' If you get an error you don't understand,
#' what should you do?

#' As we will see later, working with the raw_lines vector is
#' much easier for certain tasks, but for now let's focus on raw_text
#' as this is often the sort of text data you might have to deal with

cat(raw_text)

## This is a bit of a mess, but can you answer these questions?

#' How many Professors are listed?
#' How did you find them?
#' How are the surnames listed?
#' How about forenames?
#' How many teachers are Social Science specialists?
#' How about English specialists?
#' Are all the faculty full Professors?
#' Between which "anchors" are fields of expertise listed?

## Regular expressions (REGEX) ----

## <https://www.ecosia.org/images?q=Some%20people%2C%20when%20confronted%20with%20a%20problem%2C%20think%20%E2%80%9CI%20know%2C%20I%E2%80%99ll%20use%20regular%20expressions.%E2%80%9D%20Now%20they%20have%20two%20problems.&addon=opensearch&addonversion=7.2.0#id=4C1250D1988C835852DCE493C7B817557DCAAC34>

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
#' "[A-Z]" any uppercase letter
#' "[A-Z']+" any uppercase letter or apostrophe multiple times
#' "English|Social Science" match English or Social Science
#' "Professor|Associate Professor" match Professor or Associate Professor
#' .+?(?=\\d+|$) A lazy match until either a digit or end of the string
#' "\\b" Word boundary - matches position between word and non-word character
#' 
#' Test some of these out with str_view()

str_view(raw_text, "\\d+", html = TRUE)

#' Difficult to see because there are no linebreaks in raw_text
#' Let's add them by adding a new line before each digit

raw_view <- str_replace_all(raw_text, "(\\d+)", "\n\\1")

# What's the difference?
print(raw_text)
print(raw_view)

# What's the difference?
cat(raw_text)
cat(raw_view)

## View all digits
str_view(raw_view, "\\d+", html = TRUE)
## View matches for English or Social Science
str_view(raw_view, "English|Social Science", html = TRUE)
#' View word character matches between 
#' Professor start anchor and comma end anchor
str_view(raw_view, "(?<=Professor)\\w+(?=,)", html = TRUE)
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
str_extract_all(raw_text, "(?<=Professor).+?(?=\\sFields)")
#' What happens if you do a greedy match instead?


# extract  position of each faculty member ----
#' hint: you want to extract either Professor or Associate Professor
str_extract_all(raw_text, "Associate Professor|Professor")

# extract the fields of expertise ----
str_extract_all(raw_text, "(?<=Specialization).+?(?=\\d+)")
#' What's wrong with this?
#' hint the look ahead anchor should be a digit OR end of the string
str_extract_all(raw_text, "(?<=Specialization).+?(?=\\d+|$)")


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
str_extract(entries, "(?<=,\\s)\\w+(?=\\sFields)")

#' Extract the fields of expertise  ----
#'  we just need to anchor between
#' Specialization and the end of each string
str_extract(entries, "(?<=Specialization\\s).+$")
#' What's wrong with this?

#' Find the Social Science experts  ----
#' hint extract either Social Science or English
str_extract(entries, "Social Science|English")
#' What happens when we extract strings
#'  starting with Social Science instead?
str_extract(entries, "^Social Science")
#' What happens if you use str_detect instead?
str_detect(entries, "^Social Science")
#' What happens if you use str_subset instead?
str_subset(entries, "^Social Science")


## 3.5 FIXING TYPOS with str_replace ----

#' Did you notice the typos in the data?
#' Row 12 has "Semniar" instead of "Seminar"
#' Row 12 also has "Specilazation" instead of "Specialization"

#' Let's fix these using str_replace() and str_replace_all()

# Look at entry 12 before fixing
entries[12]

# Fix "Semniar" -> "Seminar"
entries <- str_replace(entries, "Semniar", "Seminar")

# Fix "Specilazation" -> "Specialization" 
entries <- str_replace(entries, "Specilazation", "Specialization")

# Check entry 12 after fixing
entries[12]

#' What's the difference between str_replace() and str_replace_all()?
#' Try this example:
test_string <- "The cat sat on the mat with another cat"
str_replace(test_string, "cat", "dog")
str_replace_all(test_string, "cat", "dog")

#' str_replace() only replaces the FIRST match
#' str_replace_all() replaces ALL matches

#' WORD BOUNDARIES with \\b ----
#' 
#' Word boundaries (\\b) help us match whole words only
#' This prevents partial matches inside larger words

# Example: Let's say we want to find "trade" as a complete word
test_text <- "trade policy and international trade are different from traded goods"

# Without word boundary - matches partial words too
str_view_all(test_text, "trade")
#' Notice it highlights: trade, trade, and trade (inside traded)

# With word boundary - only matches the complete word
str_view_all(test_text, "\\btrade\\b")
#' Now it only highlights the two instances of "trade" as a standalone word

#' Why is this useful? Try extracting "Law" from the entries
str_subset(entries, "law")
#' This might find "law" inside "lawyer" or "lawful" if they existed

#' If we only wanted entries where "law" appears as a complete word:
str_subset(entries, "\\blaw\\b")
#' (Note: This won't find anything in our data since "Law" is capitalized)

#' CHARACTER CLASSES for case-insensitive matching ----
#' 
#' Use [Ll] to match either uppercase OR lowercase L
#' This is useful when you want to match a word regardless of capitalization

# Find "Law" or "law" as a complete word
str_subset(entries, "\\b[Ll]aw\\b")
#' This will match both "Law" and "law" as standalone words

# You can use character classes for any letter:
test_mixed <- c("Economics is great", "economics is great", "Microeconomics")
str_subset(test_mixed, "\\b[Ee]conomics\\b")  # matches first two, not third

#' Or match the whole word with any capitalization pattern:
str_subset(test_mixed, "\\b[Ee][Cc][Oo][Nn][Oo][Mm][Ii][Cc][Ss]\\b")
#' This works but gets tedious! For longer words, you might prefer:
str_subset(test_mixed, regex("\\beconomics\\b", ignore_case = TRUE))

#' Better example with our actual data - find "Trade" as a standalone word:
test_entries <- c("Trade Policy expert", "International Trade specialist", "Traded goods analyst")
str_subset(test_entries, "Trade")  # matches all three
str_subset(test_entries, "\\bTrade\\b")  # only matches first two

#' Practice: How would you find entries with the complete word "Management" 
#' in either uppercase or lowercase?
#' Hint: Try using [Mm]anagement with word boundaries

# Your code here:



## 4 WHITESPACE CLEANING with str_trim and str_squish ----

#' Often when we extract text, we get extra whitespace
#' that we need to clean up. R has two main functions for this:
#' str_trim() and str_squish()

# Let's look at our extracted fields
fields_messy <- str_extract(entries, "(?<=Specialization).+$")
fields_messy

#' Notice all the leading spaces! Let's see them more clearly
cat(fields_messy[1])
#' There's a space at the start

#' str_trim() removes leading and trailing whitespace ----
#' (but leaves internal whitespace alone)

messy_example <- c("  hello world  ", "  R is great  ", "data   science  ")
messy_example

# Remove whitespace from both sides (default)
str_trim(messy_example)

# Remove only from left side
str_trim(messy_example, side = "left")

# Remove only from right side
str_trim(messy_example, side = "right")

#' Apply to our data
fields_clean <- str_trim(fields_messy)
cat(fields_clean[1])
#' Much better!

#' str_squish() does more: it removes leading/trailing whitespace
#' AND reduces internal whitespace to single spaces ----

messy_example2 <- c("  hello    world  ", "  R   is    great  ", "data     science  ")
messy_example2

# str_trim only fixes the edges
str_trim(messy_example2)
#' Still has multiple spaces inside

# str_squish fixes everything
str_squish(messy_example2)
#' Single spaces throughout!

#' When to use which? ----
#' 
#' Use str_trim() when:
#' - You only need to remove edge whitespace
#' - Internal whitespace is intentional/important
#' - Example: "  Economics  " -> "Economics"
#'
#' Use str_squish() when:
#' - You want to normalize all whitespace
#' - Text has inconsistent spacing
#' - Example: "  Trade   Policy  " -> "Trade Policy"

#' Practice: Create a vector with messy spacing
practice <- c("  International    Trade  ", " Economic     Development ", "Labour    Economics  ")

# Try str_trim
str_trim(practice)

# Try str_squish  
str_squish(practice)

#' Which one would you use for cleaning up the fields of expertise?


## 5 CREATE a dataframe ----

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
#' Notice we use str_squish to clean up the fields column!

ISS_prof_df %>% 
  mutate(fields = str_squish(fields)) %>% 
  separate_longer_delim(fields, ",")



## 5 PRACTICE EXERCISES ----

#' Now try your skills on this OECD country data!
#' This is formatted similarly to the ISS professor data
#' First read the file in from your data folder, and save it as oecd_text
#' You can try read_file() or read_lines() and see if you get any difference

oecd_text <- read_file("data/oecd_scrape.txt")
oecd_lines <- 

## EXERCISE 1: Basic Exploration ----
#' Compare the objects - any difference?


#' Let's work with the oecd_text object for now
#' View the text with line breaks (add \\n before each country's digit)
#' Hint: There are more numbers in this scrape, so add Country to REGEX


raw_view <- str_replace_all(oecd_text, "(\\d+Country)", "\n\\1")

cat(raw_view)

#' How many countries are in this dataset?
#' Which countries joined the OECD in 1961?
#' Which country has the highest GDP per capita?
#' How many countries have Spanish as an official language?

## EXERCISE 2: Extract from oecd_text ----

#' Extract all country names
#' Hint: Country names come after "Country: " and before " Capital:"



#' Extract all capital cities
#' Hint: Capitals come after "Capital: " and before " Population:"



#' Extract all populations
#' Hint: Look for patterns like "X.X million" after "Population: "



#' Extract GDP per capita values
#' Hint: Look for dollar amounts with commas



#' Extract the year each country joined
#' Hint: 4-digit numbers after "Joined OECD: "



## EXERCISE 3: Split and Extract ----

#' Split the text into individual country entries



#' From the split entries, extract:
#' - Country names
#' - Capitals
#' - Official languages
#' - Join years



## EXERCISE 4: Find and Replace ----

#' Suppose "Bogotá" was misspelled as "Bogota" (no accent)
#' How would you fix this?



#' Replace "million" with "M" in all population figures
#' Hint: This should affect multiple entries, so choose the right function



## EXERCISE 5: Word Boundaries ----

#' Find entries where "French" appears as a complete official language
#' (should match Belgium, Canada, and France but not French-derived words)
#' Hint: Use word boundaries



## EXERCISE 6: Create a Dataframe ----

#' Create a tibble with columns:
#' - country
#' - capital  
#' - population
#' - gdp_per_capita
#' - languages
#' - year_joined




## BONUS CHALLENGE ----

#' The population column has values like "25.7 million"
#' Convert these to numeric values (e.g., 25.7)
#' Hint: You'll need str_extract() to get the numbers, 
#' then as.numeric() to convert to numbers



#' Some countries have multiple official languages separated by commas
#' Create a "long" dataframe where each language gets its own row
#' Hint: Use separate_longer_delim()




## ANSWER KEY ----
#' Uncomment the following section to see answers

# # Exercise 1: View with line breaks
# oecd_view <- str_replace_all(oecd_text, "(\\d+)", "\n\\1")
# cat(oecd_view)
# 
# # Exercise 2: Extract country names
# str_extract_all(oecd_text, "(?<=Country: ).+?(?= Capital:)")
# 
# # Extract capitals
# str_extract_all(oecd_text, "(?<=Capital: ).+?(?= Population:)")
# 
# # Extract populations
# str_extract_all(oecd_text, "(?<=Population: ).+?(?= GDP)")
# 
# # Extract GDP
# str_extract_all(oecd_text, "(?<=GDP per capita: )\\$[\\d,]+")
# 
# # Extract join years
# str_extract_all(oecd_text, "(?<=Joined OECD: )\\d{4}")
# 
# # Exercise 3: Split into entries
# oecd_entries <- str_split_1(oecd_text, "\\d+")
# oecd_entries <- oecd_entries[oecd_entries != ""]
# 
# # Extract from split entries
# str_extract(oecd_entries, "(?<=Country: ).+?(?= Capital:)")
# str_extract(oecd_entries, "(?<=Capital: ).+?(?= Population:)")
# str_extract(oecd_entries, "(?<=Official Language: ).+?(?= Joined)")
# str_extract(oecd_entries, "(?<=Joined OECD: )\\d{4}")
# 
# # Exercise 4: Replace
# oecd_entries <- str_replace(oecd_entries, "Bogota(?!́)", "Bogotá")
# oecd_entries <- str_replace_all(oecd_entries, " million", "M")
# 
# # Exercise 5: Word boundaries
# str_subset(oecd_entries, "\\bFrench\\b")
# 
# # Exercise 6: Create dataframe
# oecd_df <- tibble(
#   country = str_extract(oecd_entries, "(?<=Country: ).+?(?= Capital:)"),
#   capital = str_extract(oecd_entries, "(?<=Capital: ).+?(?= Population:)"),
#   population = str_extract(oecd_entries, "(?<=Population: ).+?(?= GDP)"),
#   gdp_per_capita = str_extract(oecd_entries, "(?<=GDP per capita: )\\$[\\d,]+"),
#   languages = str_extract(oecd_entries, "(?<=Official Language: ).+?(?= Joined)"),
#   year_joined = str_extract(oecd_entries, "(?<=Joined OECD: )\\d{4}")
# )
# 
# # Bonus: Convert population to numeric
# oecd_df <- oecd_df %>%
#   mutate(population_numeric = as.numeric(str_extract(population, "[\\d.]+")))
# 
# # Bonus: Separate languages into rows
# oecd_df %>%
#   separate_longer_delim(languages, ", ")
