## load tidyverse
library(tidyverse)

## get see() function
source("r_docs/see_regex.R")


## Create character vectors
addresses <- c("14 Pine Street, Los Angeles", "152 Redwood Street, Seattle",
               "8 Washington Boulevard, New York")

products <- c("TV ", " laptop", "portable charger",
              "Wireless Keybord", " HeadPhones ")


long_sentences <- stringr::sentences[1:10]

field_names <- c("order_number", "order_date", "customer_email", "product_title", "amount")

employee_skills <- c("John Bale (Beginner)", "Rita Murphy (Pro)", "Chris White (Pro)", "Sarah Reid (Medium)")

mydf <- tribble(
  ~`country`, ~`Population`, ~`life expectancy`, ~`Income per person`, ~`GDP growth`, ~`trade deficit`,
  "Narnia", 43000,88,2300,10,-200,
  "Gondor", 56000,67,200,-4,400)

## REGEX LIST
"\\d+"
",\\s"
",\\s"
"(?<=\\d)\\s"
"^T\\w+\\s"
"\\w+s.$"
"(.{20}).*$"
"\\1\\.\\."
"[)]"
"\\s[(]"

## stringr list
## str_extract
## str_to_lower
## str_split
## str_extract_all
## str_remove
## str_replace
## str_replace_all
## str_trim
## str_to_upper
## janitor::clean_names


# Exercise 1 ----
# Normalize the addresses vector by replacing capitalized letters with lower-case ones.
str_to_lower(addresses)

## In case of a tibble

addresses %>% 
  ## convert vector to tibble
  tibble(address = .) %>% 
  mutate(address = str_to_lower(address))

# Exercise 2 ----
# Extract only the numeric part of the addresses vector.

## check regex with see()
see("\\d", string = addresses)
see("\\d+", string = addresses) # note the difference

## with str_extract
addresses %>% str_extract("\\d+")
addresses %>% str_extract("\\d+") %>% class # character
addresses %>% str_extract("\\d+") %>% as.integer()

## alternative with readr::parse_number()
addresses %>% parse_number 

## In case of a tibble

addresses %>% 
  ## convert vector to tibble
  tibble(address = .) %>% 
  mutate(number = parse_number(address))

# Exercise 3 ----
# Split the addresses vector into two parts: address and city. The result should be simplified to a matrix.

## Hint: find a place to split the string

## check regex with see()
see("," , string = addresses)
see(",\\s", string = addresses)


## note this is a matrix output
addresses %>% 
  str_split(pattern = ",\\s", simplify = TRUE)


## for tibble output use as_tibble() on a matrix
addresses %>% 
  str_split(pattern = ",\\s", simplify = TRUE)  %>% 
## convert to tibble from matrix
as_tibble() %>% 
  ## set the column names
  set_names(c("address","city"))

## in case of a tibble, use tidyr::separate() 
addresses %>% 
  tibble(address = .) %>% 
  separate(address, into = c("address", "city"), sep = ",\\s")


# Exercise 4 ----
# Now try to split the addresses vector into three parts: house number, street and city. The result should be a matrix.
# Hint 1: find 2 places to split the string
# Hint 2: use a regex lookbehind assertion

## check regex with see()
see(",\\s", string = addresses)
see("\\d\\s" , string = addresses) # captures the last digit as well
see("(?<=\\d)\\s", string = addresses) # using lookbehind regex
see("(?<=\\d)\\s|,\\s", string = addresses) # use OR pipe | 

addresses %>% 
  str_split(pattern = "(?<=\\d)\\s|,\\s",simplify = TRUE) %>% 
  as_tibble() %>% 
  set_names(c("number","street","city"))

## in case of a tibble, use tidyr::separate() 
addresses %>% 
  tibble(address = .) %>% 
  separate(address, into = c("number","street", "city"), sep = "(?<=\\d)\\s|,\\s")

## sometimes easier to do two steps
addresses %>% 
  tibble(address = .) %>% 
  separate(address, into = c("address", "city"), sep = ",\\s") %>% 
  separate(address, into = c("number", "street"), sep = "\\s", extra = "merge")
## What happens without the the extra = "merge" argument?







# Exercise 5 ----
# In the long_sentences vector, for sentences that start with the letter "T" or end with the letter "s", show the first or last word respectively.


## check regex with see()
## string starts with "T" plus following word characters
see(rx = "^T\\w+",long_sentences)
## last word ending in "s" plus period
see(rx = "\\w+s\\.$",long_sentences)

## use a lookahead regex to not capture the final period
## use OR pipe to capture both
see(rx = "^T\\w+|\\w+s(?=\\.$)",long_sentences)

# If the sentence both starts with a "T" and ends with an "s", show both the first and the last words.

## two steps without lookahead regex
long_sentences %>%  
  str_extract_all("^T\\w+|\\w+s\\.$") %>% 
  unlist() %>% 
  str_remove("\\.$")

## One step with the lookahead regex
long_sentences %>%  
  str_extract_all("^T\\w+|\\w+s(?=\\.$)") %>% 
  unlist() 

## In a tibble - creating new columns
long_sentences %>% 
  tibble(sentence = .) %>% 
  mutate(start_T = str_extract(sentence, "^T\\w+")) %>% 
  mutate(end_s = str_extract(sentence, "\\w+s(?=\\.$)"))

# Exercise 6 ----
# Show only the first 20 characters of all sentences in the long_sentences vector. 
# To indicate that you removed some characters, use two consecutive periods at the end of each sentence.

# check regex with see()
see(".{20}", string = long_sentences)


long_sentences %>% 
  ## capture first 20 in one group, replace with first group plus ..
  str_replace_all("(.{20}).*$","\\1\\.\\.")

## alternative with str_trunc
long_sentences %>% 
  str_trunc(width = 20, side ="right", ellipsis = "..")

## In a tibble
long_sentences %>% 
  tibble(sentence = .) %>% 
  mutate(first_20 = str_replace(sentence, "(.{20}).*$","\\1\\.\\.")) 

# Exercise 7
# Normalize the products vector by removing all unnecessary whitespaces (both from the start, the end and the middle), 
# and by capitalizing all letters.

## check the whitespaces
see("\\s", string = products)
## check for double whitespaces
see("\\s\\s", string = products)

## str_trim()
products %>% str_trim %>% str_to_upper
# str_squish()
products %>% str_squish %>% str_to_upper

# Exercise 8
# Convert the df_column names to "tidy" versions with janitor::clean_names()
mydf
janitor::clean_names(mydf)

# Exercise 9
# Prepare the field_names for display, by replacing all of the underscore symbols with spaces, and by converting it to the title-case.

## check regex with see()
see("_", string = field_names)
see("^.", string = field_names)
see("(?<=_).", string = field_names)

## use str_to_title()
field_names %>% str_replace("_"," ") %>% str_to_title


# Exercise 10
# Create a matrix that has the employee name in the first column,
# and the skill level (without parenthesis) in the second column.

## check regex with see()
see("\\)", string = employee_skills)
see("\\(", string = employee_skills)
see("\\s\\(", string = employee_skills)

## Two step process - remove the rhb then split on space and lhb
employee_skills %>% 
  ## remove rhb
  str_remove("\\)") %>% 
  ## split string on space and lhb - simplify for matrix output
  str_split("\\s\\(", simplify = TRUE)


## Convert from matrix to tibble with as_tibble()
employee_skills %>% 
  ## remove rhb
  str_remove("\\)") %>% 
  ## split string on space and lhb - simplify for matrix output
  str_split("\\s\\(", simplify = TRUE) %>% 
  ## convert to tibble from matrix
  as_tibble() %>% 
  ## set the column names
  set_names(c("name","skill_level"))


## Alternative using tidyr::separate()
employee_skills %>% 
  ## remove rhb
  str_remove("[)]") %>% 
  ## create tibble from vector
  tibble(data = .) %>% 
  ## separate into two columns
  separate(data, into = c("name", "skill_level"), sep = "\\s\\(")
