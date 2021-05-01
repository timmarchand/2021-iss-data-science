
# Week 2. Baby steps: simple maths with R ####
# Note the text after the # is just comments and not read by R
# Most of the following code can be completed in Base R


# Addition:

2 + 2

# Subtraction:

3 - 2

# Incomplete command; abort with ESC:

3 - 1

# Division:

3 / 2

# Multiplication:

3 * 2

# Two, squared (2 * 2):

2 ^ 2

# Two, to the power of three (2 * 2 * 2):

2 ^ 3

# Two pluts three (=5), multiplied by 3:

(2 + 3) * 3

# Three times three (=9) plus 2:

2 + (3 * 3)

# Square root of four:

sqrt(4)

# Not supplying an argument gives error:

sqrt(9)

# Absolute value makes negative numbers positive:

abs(-2)
abs(2)


# 1.2. Assigning variables to objects: ####

# Assign the output of 2 * 3 to the object named 'x':

x <- 2 * 3 # You can read this as "Take x and give it 2 times 3" or "x gets 2 times 3"

# Print object name to check content of object 'x':

x

# Another way of assigning, less preferred:

x = 2 * 3 
# You can also read this as "Take x and give it 2 times 3". 
# However, it is best avoid using the equals sign when assigning values, because you will be using it in other types of code 



# Use 'x' as if it's a number:

x / 2

# R is case-sensitive, so capital 'X' yields an error...
# ... because the object doesn't exist:

X

# Check all R objects of current working environments:

ls()

## more examples... feel free to add your own comments!

result <- 5 + 3
result
print(result)
result <- 5 - 3
result

tim <- "teacher"
tim
tim <- "teacher and tea-maker"
tim

Result <- "5"
Result
result

class(result)
Result
class(Result)
class(sqrt)

# 1.3. Numeric vectors ####



# Assign three numbers to 'x', combined with c():

x <- c(2.3, 1, 5)
x

# Check how many numbers are in this vector:

length(x)

# Checking the type of vector:

mode(x)
class(x)

# Colon creates an integer sequence from...
# ... the first argument (10) to the second (1):

mynums <- 10:1
mynums

# Sum:

sum(mynums)

# Find the minimum value:

min(mynums)

# Find the maximum value:

max(mynums)

# Find the minimum and maximum together:

range(mynums)

# Find the difference between minimum and maximum:

diff(range(mynums))

# Arithmetic mean

mean(mynums)

# Standard deviation

sd(mynums)

# Median

median(mynums)

# Subtract 5 from every number:

mynums - 5

# Divide every number by two:

mynums / 2



# 1.4. Indexing: ####

# Retrieve value at first position:

mynums[1] # This can be read as "mynums when it is in the first position"

# Retrieve value at second position:

mynums[2]

# Retrieve first four values:

mynums[1:4]  # This can be read as "mynums when it is in the first to fourth position"

# Retrieve everything except second position:

mynums[-2] # the minus sign means return everything EXCEPT the elelment in the 2nd position

# 100 numbers, notice the square brackets [] to the left...
# ... of the console; these list the next position:

1:100

# practice with real data 1 - feel free to add comments! ####
# this is a comment
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)
world.pop

# using c()
pop.first <- c(2525779, 3026003, 3691173)
pop.second <- c(4449049, 5320817, 6127700, 6916183)
pop.all <- c(pop.first, pop.second)
pop.all

# check to see if they are the same
all.equal(world.pop,pop.all)

# indexing
world.pop[2]
world.pop[c(2, 4)] 
world.pop[c(4, 2)] 
world.pop[-3]

# manipulating
pop.million <- world.pop / 1000
pop.million

pop.rate <- world.pop / world.pop[1]
pop.rate

# what's happening here?
pop.increase <- world.pop[-1] - world.pop[-7]
percent.increase <- (pop.increase / world.pop[-7]) * 100
percent.increase
percent.increase[c(1, 2)] <- c(20, 22)
percent.increase



# 1.7. Logical vectors: ####

# Logical statement: Is the value is larger than 3?

mynums > 3 # Notice how this returns TRUE/FALSE values for every position in the vector

# Is it larger than or equal to 3?

mynums >= 3 # This can be read as a question: "Is mynums greater or equal to three?"

# Smaller than 4?

mynums < 4

# Smaller than or equal to 4?

mynums <= 4

# Equal o 4?

mynums == 4 # Note how double equal signs are used!! 

# Not equal to 4?

mynums != 4

# Save 'Larger than or equal to 3?' output to...
# ... vector called 'mylog':

mylog <- mynums >= 3 
# This can be read as "Take mylog, and give it the answer to is mynums greater or equal to three"

# Check what type of vector this is:

class(mylog)

# Use this vector for indexing:

mynums[mylog] 
# This can be read as "mynums, when indexed by mylog"
# If you remember the value of mylog, then this means "mynums when its value is greater or equal to three" 

#Same in one step, using logical statement ...
# ... to index specific values:

mynums[mynums >= 3] # Same reading as above: "mynums when its value is greater or equal to three"




# 1.8. Character vectors: ####

# Create a character vector with 5 gender identifiers:

gender <- c('F', 'M', 'M', 'F', 'F') # Can be read as "Take gender and give it a combination of 'F', 'M', 'M', 'F', 'F'"

# Check content of vector:

gender   # notice the quotation marks

# Check vector class:

class(gender)

# As before, you can use positions for indexing ...

gender[2]

# ... or logical statement:

gender[gender == 'F']

# Being a character vector, certain mathematical ...
# ... operations 

mean(gender)




# 1.9. Factor vectors: ####

# Convert the character vector 'gender' to a factor:

gender <- as.factor(gender) # This can be read as "take gender and give it the value of gender as factors"

# Check:

gender   # notice the absence of quotation marks

# Retrieve levels (unique types):

levels(gender)

# Once the levels are fixed, you can't just add new ones:

gender[3] <- 'non_binary'
gender   # notice the NA (missing value)

# Assigning new levels:

levels(gender) <- c('M', 'F', 'non_binary')

# Now it's possible to add the new text:

gender[3] <- 'non_binary'
gender



# 1.10. Data frames: ####

# Create a new vector of names:

participant <- c('louis', 'paula', 'vincenzo')

# Put this into a dataframe ...
# ... the second column is named 'score' and we assign ...
# ... three numerical values:

mydf <- data.frame(participant, score = c(67, 85, 32))
# This can be read as "Take mydf and give it a data frame with participant and score, a combination of 67, 85, 32"

# Check data frame:

mydf

# Ask about the number of rows:

nrow(mydf)

# Ask about the number of columns:

ncol(mydf)

# Ask about the column names:

colnames(mydf)

# Columns can be indexed with column name and '$':

mydf$score # This can be read as "extraction of score from mydf"

# Compute the mean of the score column:

mean(mydf$score)

# Retrieve structure of the data frame:

str(mydf)

# Summarize the data frame:

summary(mydf)

# Index first row, no column restrictions:

mydf[1, ] # This can be read as "mydf when it is the first row"

# Note: everything before the comma relates to rows...
# ... everything after comma to columns.

# Index second column:

mydf[, 2] # "mydf when it is the second column"

# First two rows:

mydf[1:2, ]

# Stacking indexing statements: First column, second entry:

mydf[, 1][2] #"mydf when it is the first column and second entry"

# Extract row with participant 'vincenzo':

mydf[mydf$participant == 'vincenzo', ] # mydf when the participant column equals 'vincenzo'

# Extract this participant's score:

mydf[mydf$participant == 'vincenzo', ]$score 
# Extraction of score when when the participant column equals 'vincenzo'

# Practice with real data 2 - Learning some funtions ####
length(world.pop)  
min(world.pop)     
max(world.pop)     
range(world.pop)   
mean(world.pop)    
sum(world.pop) / length(world.pop) 

year <- seq(from = 1950, to = 2010, by = 10)
year

seq(to = 2010, by = 10, from = 1950)

seq(from = 2010, to = 1950, by = -10)
2008:2012
2012:2008

names(world.pop) 
names(world.pop) <- year
names(world.pop)
world.pop

## myfunction <- function(input1, input2, ..., inputN) {
## 
##     DEFINE `output' USING INPUTS
## 
##     return(output)
## }

my.summary <- function(x){ # function takes one input
  s.out <- sum(x)
  l.out <- length(x)
  m.out <- s.out / l.out
  out <- c(s.out, l.out, m.out) # define the output
  names(out) <- c("sum", "length", "mean") # add labels
  return(out) # end function by calling output
}
z <- 1:10
my.summary(z)
my.summary(world.pop)

# 1.11. Loading in files: ####

#Check current working directory:

getwd()	# output specific to your computer
# Read as "get working directory"

# List files in the current working directory:

list.files() # output specific to your computer

# Change working directory to where the file...
# 'nettle_1999_climate.csv' is located at...
# ... in RStudio: Click on 'Session' tab, ...
# ... then 'Set working directory', navigate to folder.

# Load in the Nettle (1999) linguistic diversity data after setting the wd:

nettle <- read.csv('nettle.csv')

# Better to load in the nettle data with a relative path:

nettle <- read.csv('data/nettle.csv')

# Whenever loading a new file, check its content.
# First six rows:

head(nettle)

# Last six rows:

tail(nettle)

# Or add an argument for the number of rows you want to see
tail(nettle, 10) # see the last 10 rows

# Have a look at all variables using glipmse()
library(dplyr)
glimpse(nettle)

# Have a look at the dataframe in a spreadsheet format
View(nettle)

 # Accidentally overriding the 'Langs' columns with NAs...
# ... is not a problem, because you can just ...
# ... load the file back into R.

nettle$Langs <- NA

# Reading a tab-delimited file:

mydf <- read.table('data/example_file.txt',
                   sep = '\t', header = TRUE)
                   
# Check:

mydf

# To find out the structure of a file, load it into R...
# ... as text (here, only first two lines: n = 2):

x <- readLines('data/example_file.txt', n = 2)
x

# The output reveals '\t' tab delimiters.

## Practice with real data 3 -  examples with UNpop ####

UNpop <- read.csv("data/UNpop.csv")
class(UNpop)

# choose a way to view the data: head() / glimpse() / View()

load("data/UNpop.RData")

names(UNpop)
nrow(UNpop)
ncol(UNpop)
dim(UNpop)
summary(UNpop)

UNpop$world.pop

UNpop[, "world.pop"] # extract the column called "world.pop"
UNpop[c(1, 2, 3),]   # extract the first three rows (and all columns)
UNpop[1:3, "year"]   # extract the first three rows of the "year" column



## take elements 1, 3, 5, ... of the "world.pop" variable
UNpop$world.pop[seq(from = 1, to = nrow(UNpop), by = 2)]

world.pop <- c(UNpop$world.pop, NA)
world.pop
mean(world.pop)
mean(world.pop, na.rm = TRUE)



# 1.12. Basic Plotting: ####

# Create a histogram:

hist(nettle$Langs)

# Create a salmon-colored histogram:

hist(nettle$Langs, col = 'salmon')

# Notice that 'col' is an optional argument ...
# ... (the function works fine without).

# Retrieve names of all colors pre-defined in R:

colors()


# 1.13. Installing and loading packages: ####

# Install a package, select server:

install.packages('car')

# Load installed package into current R session:

library(car)

## install.packages("foreign") # install package
## library("foreign") # load package
## 
## read.dta("UNpop.dta")
## read.spss("UNpop.sav")
## 
## write.dta(UNpop, file = "UNpop.dta")



# 1.14. Seeking help: ####

# Access help file of ?seq function:

?seq

## practice using swirl() ####

## install.packages("swirl") # install the package
## library(swirl) # load the package
## install_course_github("kosukeimai", "qss-swirl") # install the course
## library(swirl)
## swirl()




