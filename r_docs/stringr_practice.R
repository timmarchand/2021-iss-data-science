## load library
library(tidyverse)
library(janitor)

## load function
source("r_docs/see_regex")


## some variables and data

addresses <- c("14 Pine Street, Los Angeles", "152 Redwood Street, Seattle",
               "8 Washington Boulevard, New York")

products <- c("TV ", " laptop", "portable charger",
              "Wireless Keybord", " HeadPhones ")

mydf <- tribble(
                ~`country`, ~`Population`, ~`life expectancy`, ~`Income per person`, ~`GDP growth`, ~`trade deficit`,
                "Narnia", 43000,88,2300,10,-200,
                "Gondor", 56000,67,200,-4,400)

long_sentences <- stringr::sentences[1:10]


field_names <- c("order_number", "order_date", "customer_email", "product_title", "amount")

employee_skills <- c("John Bale (Beginner)", "Rita Murphy (Pro)", "Chris White (Pro)", "Sarah Reid (Medium)")

cat(employee_skills,sep = "\n")

## REGEX LIST
## You will need to input some of these REGEX in the code below
## Use see() to find out what each one matches
# "\\d+"        >>> any digit (repeated) (e.g. "3" or "32" or "323")
# ",\\s"        >>> any comma followed by a whitespace (e.g. ", ")
# "^T\\w+\\s"
# "\\w+s.$"
# "(.{20}).*$"
# "\\1\\.\\."
# "[)]"
# "\\s[(]"
# "(?<=\\d)\\s"


## stringr list
# Here are a list of stringr functions you can use in the following exercise
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


## Use the REGEX list and list of functions to help you answer these 10 exercises

# Exercise 1
# Normalize the addresses vector by replacing capitalized letters with lower-case ones.

# Exercise 2
# Extract only the numeric part of the addresses vector.


# Exercise 3
# Split the addresses vector into two parts: address and city. The result should be simplified to a matrix.


# Exercise 4
# Now try to split the addresses vector into three parts: house number, street and city. The result should be a matrix.
# Hint: use a regex lookbehind assertion and an or pipe "|"

# Exercise 5
# In the long_sentences vector, for sentences that start with the letter "T" or end with the letter "s", show the first or last word respectively.
# If the sentence both starts with a "T" and ends with an "s", show both the first and the last words.
# Remember that the actual last character of a sentence is usually a period.


# Exercise 6
# Show only the first 20 characters of all sentences in the long_sentences vector. 
# To indicate that you removed some characters, use two consecutive periods at the end of each sentence.


# Exercise 7
# Normalize the products vector by removing all unnecessary whitespaces (both from the start, the end and the middle), 
# and by capitalizing all letters.


# Exercise 8
# Convert the df_column names to "tidy" versions with janitor::clean_names()


# Exercise 9
# Prepare the field_names for display, by replacing all of the underscore symbols with spaces, and by converting it to the title-case.



# Exercise 10
# Create a matrix that has the employee name in the first column,
# and the skill level (without parenthesis) in the second column.
