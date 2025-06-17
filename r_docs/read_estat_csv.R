
## Try out read_csv on the first file
read_csv("data/estat_csvs/Aichi_ken.csv")

## Biggest issue is encoding! Need to set the
## encoding to one that can handle Japanese characters

read_csv("data/estat_csvs/Aichi_ken.csv",
         locale = locale(encoding = "CP932")) # Or "Shift-JIS")

## read_csv has included all rows from the file
## But a few elements are for human reading rather than
## computer reading - so let's skip the first 6 rows

test <- read_csv(
  "data/estat_csvs/Aichi_ken.csv",
  skip = 6,
  locale = locale(encoding = "CP932")) # Or "Shift-JIS"


glimpse(test)

# looks like we don't need the first 2 columns
# let's un-select them

test <-  test %>% 
  select(-c(1,2))

# Or select the columns when reading in the data

read_csv(
  "data/estat_csvs/Aichi_ken.csv",
  skip = 6,
  locale = locale(encoding = "CP932"),# Or "Shift-JIS
  col_select = c(3:17)) # select columns 3-17


names(test)

# How would you translate them to English?

# 地域コード            
# 地域識別コード        
# 境域年次(2015)        
# 境域年次(2000)        
# ...7                  
# 人口　総数            
# 人口　男              
# 人口　女              
# 人口性比              
# 世帯数　総数          
# 世帯数　一般世帯      
# 世帯数　施設等の世帯  
# 世帯人員　総数        
# 世帯人員　一般世帯    
# 世帯人員　施設等の世帯

## Here are some new column names in "snake case"
column_names <- c(
  "region_code",
  "region_identifier_code",
  "boundary_year_2015",
  "boundary_year_2000",
  "region_name",
  "population_total",
  "population_male",
  "population_female",
  "sex_ratio",
  "households_total",
  "households_general",
  "households_institutional",
  "household_members_total",
  "household_members_general",
  "household_members_institutional"
)

## set the column names to our test df
test <- test %>% 
          set_names(column_names) 
test

## If we are happy with that, pull all the steps into one function

read_estat_csv <- function(file, 
                           skip = 6, # set defaults, but can be changed
                           locale = readr::locale(encoding = "CP932",),
                           col_select = c(3:17)){
  
column_names <- c("region_code", "region_identifier_code", "boundary_year_2015", 
                  "boundary_year_2000", "region_name", "population_total", 
                  "population_male", "population_female", "sex_ratio", 
                  "households_total", "households_general", "households_institutional", 
                  "household_members_total", "household_members_general", 
                  "household_members_institutional")
  
  readr::read_csv(
    file = file,
    skip = skip,
    locale = locale,
    col_select = col_select,
    show_col_types = FALSE ) %>%  # Suppress message
  set_names(column_names)
}


read_estat_csv("data/estat_csvs/Aichi_ken.csv")

## Now to do it in bulk
## From the bulk_read_csv file, we replace read_csv()
## with our custom function read_estat_csv()


# set the path to your folder containing csvs
folder_path <- "data/estat_csvs"

# create a list of all the dataframes
# note list is safer at this stage as it allows for objects with different
# structures
data_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE) %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(read_estat_csv)



names(data_list)

# To access a single data file from the list, use the name inside the file name [[]]
data_list[["Gumma_ken"]]  # Access data from "mydatafile.csv"

# Read and combine .csv files of the same structure -----
combined_data <-  data_list %>% 
  bind_rows(.id = "source_file")

# We get an error here... why??

#' Can't combine `Aichi_ken$region_code` <double> and 
#' `Akita_ken$region_code` <character>

data_list[["Aichi_ken"]] %>% 
  glimpse()

data_list[["Akita_ken"]] %>% 
  glimpse()

## Check the data types for each column
## Which ones don't match?

data_list[["Akita_ken"]] %>% 
  pull(region_code) %>% 
  unique()

## Region code as a character string is okay

data_list[["Akita_ken"]] %>% 
  pull(households_institutional) %>% 
  unique()
## How about households_institutional as a character string?



#' Solution 
#' Set all columns as character strings by default
#' Then convert later


read_estat_csv1 <- function(file, 
                           skip = 6, # set defaults, but can be changed
                           locale = readr::locale(encoding = "CP932",),
                           col_select = c(3:17)){
  
  column_names <- c("region_code", "region_identifier_code", "boundary_year_2015", 
                    "boundary_year_2000", "region_name", "population_total", "population_male", 
                    "population_female", "sex_ratio", "households_total", "households_general", 
                    "households_institutional", "household_members_total", "household_members_general", 
                    "household_members_institutional")
  
  readr::read_csv(
    file = file,
    skip = skip,
    locale = locale,
    col_select = col_select,
 #   na = c("-",""), # give NA to empty strings and "-" !!!!!
    col_types = cols(.default = col_character()),  # All character as default
    show_col_types = FALSE  # Suppress message
  ) %>% 
    set_names(column_names)
}


# Read all .csv files into a named list of tibbles
data_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE) %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(read_estat_csv1) ## replacing read_csv()


# Combine all data frames from the list into 1 large df
combined_data <- data_list %>%
  bind_rows(.id = "source_file") %>% # creates an id column based on the file name
  type_convert() ##  automatically guesses the data type for the combined data

glimpse(combined_data)

#' Why have some columns been guessed as character?
#' Look at the Fukui_ken csv - what do you see in the original
#' 世帯数　施設等の世帯 column?

#' We can check our data_list too
data_list[["Fukui_ken"]] %>% 
  View()

# Is this the same elsewhere?
data_list %>% 
  map(~ which(.x == "-", arr.ind = TRUE))

#' The csv contains "-" to indicate no value
#' Easy to read for a human, but confusing for a computer!
#' We can switch that to NA instead


## Now un-comment the line highlighted with !!!!! in the function above
## This should now work as expected

#' Solution 2
#' Similar to above, but fix the arguments as parameters within the function 
#' relocate region_name column to front of data frame
#' and mutate from column4:column15 as.numeric

read_estat_csv2 <- function(file) {
  readr::read_csv(
    file = file,
    skip = 6,
    locale = locale(encoding = "CP932"),
    na = c("-", ""),
    col_select = 3:17,
    col_types = cols(
      .default = col_character()  # First read all as text
    ),
    show_col_types = FALSE
  ) %>%
    set_names(c(
      "region_code", "region_identifier_code", "boundary_year_2015", 
      "boundary_year_2000", "region_name", "population_total", "population_male", 
      "population_female", "sex_ratio", "households_total", "households_general", 
      "households_institutional", "household_members_total", 
      "household_members_general", "household_members_institutional"
    )) %>%
    relocate(region_name, .before = region_code) %>% 
    mutate(across(boundary_year_2015:household_members_institutional, as.numeric))
}

data_list2 <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE) %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(read_estat_csv2) ## replacing read_csv()

combined_data2 <- data_list2 %>%  
  bind_rows(.id = "Prefecture") # better name for this column?
combined_data2


## We could do this in one go too, even saving to file
list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE) %>%
  set_names(~ tools::file_path_sans_ext(basename(.))) %>%
  map(read_estat_csv2) %>%  ## replacing read_csv()
  bind_rows(.id = "Prefecture") %>% 
  write_csv("data/2015_Japan_population.csv")

