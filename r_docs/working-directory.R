## find where the working directory is ----
getwd()

## set working directory ----


## 1. Use a line of code
 setwd("~/Desktop/r_projects/iss_data_science")
 
## 2.  Use Seesion from top menu
# Go to  Session > Set Working Directory > Choose Directory > [FIND THE FOLDER YOU WANT]

## 3.  Use Files pane (borrom right corner)
# [FIND THE FOLDER YOU WANT]  > More > Set As Working Directory
 

## Using working directory to find files ----

## Save your working directory as a variable 
path <- getwd()
 
## list folders in working directory
 list.files(path)
 
## list files in wd
 list.files(path, recursive = TRUE)

 
## Reading files from working directory
 
## read in any csv file
 nettle <- readr::read_csv("data/nettle.csv")
 