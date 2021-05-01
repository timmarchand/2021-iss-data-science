# create a length 1 character vector called message (any message is fine)
message <- "R is my new hobby"

# use the function nchar() on the variable message - what does it
# return?
nchar(message)

# install the package “phonenumber”
install.packages("phonenumber")
# load the package “phonenumber”
library(phonenumber)
# use the function letterToNumber() on your variable message
# create a new vector called mynumber with the result of

mynumber <- letterToNumber(message)


# print() mynumber without quotation marks and copy the result to the

print(mynumber,quote = F)


# create a dataframe called df, with three columns: message, n_char, 
# using Base R
df <- data.frame(message = message,
                 n_char = nchar(message),
                 tel_code = mynumber)
# have a look at the dataframe
df
View(df)

# use tibble function
library(tibble)
mytib <- tibble(message = message,
                 n_char = nchar(message),
                 tel_code = mynumber)
mytib

# call tibble functions without using library
another_tib <- tibble::tibble(message = message,
                 n_char = nchar(message),
                 tel_code = mynumber)

# is it the same?
all_equal(mytib,another_tib)
all_equal(mytib,df)


