## Tutorial Week 1 Code 
# Author: Amber DeJohn
# Date: 2020-09-23

# If you ever need help, R has a built in function to search for answers!
help(setwd)
?setwd
# more information about the help function can be found using this:
help(help)

# Now that you know how to get help in R, we can start writing our code by setting a working directory
# if you're unsure what directory you're in, you can ask R with the following:
getwd()
# to tell R which folder on your computer you'll be referencing, use the following:
setwd("~/Dropbox/GGR270/Tutorial_Workbooks")

# depending on what you'll be doing in R, you may need special R packages called libraries
# let's install and load a library called 'dplyr'
install.packages('dplyr')
library(dplyr)

# Now that we have (1) set up a folder to work from and (2) made sure we have libraries installed/loaded,
# we can start doing some basic math (using R as a calculator)

## addition
2 + 2 #notice that the answer appears in the console

## subtraction
10 - 5

## multiplication
37 * 45

## division
78/3

## exponentation 
# there are two approaches here:
3^2 # this is the carrot approach
3**2 

## modulo
# (this gives you the remainder after dividing)
10%%3
4%%2 # no remainder here because 4 is a multiple of 2

### Now that we know the basic signs, let's assign variables with values
2 + 2 # this will return 4, but what if we want to do something with the result?
x <- 2 + 2 # now, whenever we type x, R will read it as 4
# ^ notice how, when we assign a variable, it appears in our Environment, not in the Console
# To see the value of x in the console:
x 
# or
print(x)

y <- x^2 # variables can be the result of some computation (shown here)

# now, we can build more complex equations using the variables we've assigned 
z <- x + 2*y

# if you want to know the value assigned, you can look in your Environment or call it in your code:
z
print(z)
x + y + z
print(x+y+z) # print can handle computations as well

# variable names can be long, short, gibberish, or meaningful
# best practice is to use variable names that are shorter and meaningful
myage <- 20

# variables don't necessarily have to be numbers:
myname <- "Amber" # more on this later!

# and they don't have to be one item:
petnames <- c("Navi","Sassy","Foster")
mypets <- c("Cat","Dog","Snake")

mypets
petnames 

# As you can see, R can handle many different types of things, numbers, words, etc
# These are called **Data Types** and it's important to understand how they work

###############################
##### Data Types in R #########
###############################

### 1. Decimals (also called Double/Float Numerics) ##
decimal <- 5.5 # right now, "decimal" is a data type decimal numeric

### 2. Integers (also called Numerics, Natural Numbers) ##
# Integers are whole numbers. If you tell R something is an Integer, that number
# can NEVER have a decimal. R will automatically round 
x <- x + 0.2
integer <- as.integer(x) # here, we're telling R to read the decimal variable as an integer
integer # thus, we've created a variable that is a whole number from a previously decimal number
x #decimal is has remained the same, we didn't change it's value

### 3. Booleans (also called logicals) ##
# Sometimes, you want to know if something about your data are true or false to inform your analysis
# I'm going to create a list of random integers I haven't seen
list <- sample.int(100,10)

# I can use Booleans to inform myself about the contents of the list I just created 
#set.seed(1)
bool_list <- list > 50 # I'm asking R to tell me if each entry in the list is greater than 50
bool_list # as you can see, a few values in my list are greater than 50 
# What if I want to know if there are values equal to 30?
list == 30 
# To use or create Booleans, there has to be some logical test happening so that R can return T/F values
# some data will come with booleans as well (usually 0/1 for no/yes) and 
# some functions in R require boolean inputs... more on this later! 

### 4. Strings (also called text or characters) ###
# In order to tell R the data are text/string, you MUST use quotation marks
myname <- "Amber" # R reads Amber as a string/text variable, not a decimal/integer/boolean
introduction <- "My name is Amber and I live in Toronto" # Characters can be single words or sentences
myage <- "24" # Stings can have or be numbers, but you can't do math with them!!
24**2
myage**2 # notice that R returns an error when you try to perform mathematical function on a string

##### ****DATA TYPE ERRORS ARE AMONG THE MOST COMMON!!**** ##########
# You can always check what type a variable is:
class(myage)
class(bool_list)

# If you want to change a data type:
myage <- as.integer(myage)
class(myage) # myage variable is no longer a string/character data type, it is now integer
myage**2 # it works now because we changed the data type of myage

#################################################################################################
# Now that you know the basic data types in R, you can make more complex **Data Structures**

### Create a Vector: a vector is a one dimensional array. You've already seen one!
petnames # this is a character vector. I'll show you how to create one!
ice_cream <- c("chocolate","vanilla","strawberry")
# Because ice_cream has strings/characters in it, we would call it a ** Character Vector **

test_scores <- c(92,85,79,90,100,30)
# test_scores contains 4 integer test scores. It is a ** Numeric Vector ** 

# We can name items in a vector to make it easier to remember! 
# remember mypets and petnames?
mypets
petnames

# Let's associate the pet names to the animal! 
names(mypets) <- petnames

# let's see if it worked:
print(mypets)

# we can also name the ice cream flavors to indicate preference:
names(ice_cream) <- c("favourite","ok","pretty good")
ice_cream

#### Let's use what we know to do a challenge: 
## 1. Create a vector of 10 numbers and assign to a variable
## 2. Reorder the list from smallest to largest
# with your list, calculate Mean and Median! 



# Answer: 
mylist <- sample.int(100,10)
mylist # to look at the data; could also use print(mylist)
?sort #use the help function to look up how to arrange the data
sort(mylist,decreasing = FALSE) # Note a logical/boolean value is needed to inform how data are sorted
sortedlist <- sort(mylist,decreasing = FALSE) 

mean(mylist)
median(mylist)

# you can also use the summary function:
summary(mylist)
?summary

# Make sure to save your script!

