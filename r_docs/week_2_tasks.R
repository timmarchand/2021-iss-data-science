# Task 1: practice with basic R syntax ####

# Create a variable `hometown` that stores the city in which you were born


# Assign your name to the variable `my_name`


# Assign your height (in cm) to a variable `my_height`


# Create a variable `puppies` equal to the number of puppies you'd like to have


# Create a variable `puppy_price`, which is how much you think a puppy costs in yen


# Create a variable `total_cost` that has the total cost of all of your puppies


# Create a logical variable `too_expensive`, set to TRUE if the total cost is greater 
# than ¥100000


# Create a variable `max_puppies`, which is the number of puppies you can 
# afford for ¥100000



# Task 2: trouble-shooting: errors and warnings ####

# The following code produces two error messages.
# Run the code first to have a look at the error messages, and try to fix it:

x_values <- c(1, 2 3, 4, 5, 6, 7, 8, 9)
mean_x <- mean(X_values)


# The following two lines return a warning message, and try to fix it:

x <- c(2, 3, 4, '4')
mean(x)

# Problems with a data frame

# Load the data

nettle <- read.csv('nettle.csv')

# Extract the country Yemen from the data

nettle[nettle$Country = 'Yemen', ]

# Task 3: working with the nettle dataframe ####

### use the nettle dataframe you have just created

# Check the contents of the dataframe:

# Use head() to look at the first 10 rows in the dataframe

# Choose another way to get an overall idea of the data

# Extract the second row ('Angola'), fifth column ('Langs'):

# Extract the first four rows:

# Extract first four rows, first two columns:

# Extract row for 'Bangladesh':

# Extract row for 'Bangladesh', fifth column:





