## GGR270 Tutorial 2
# Author: Your Name
# Date: The Date 

# Install new packages, then load libraries 
install.packages("ggplot2")
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(tidyverse)

# Set Working Directory 
setwd("~/Your/File/Path/Here")

## Open data file in R ##
# Note: since we set our working directory, we don't have to provide a file path to our CSV
# as long as the CSV is in our working directory folder!
read_csv("groundhog_day_temps.csv") # notice if we run this, it displays the data in the Console

# But we want to add the data to the Global Environment so we can reference it: 
temps <- read_csv("groundhog_day_temps.csv") # The data have been read into R and are assocaited with the name "temps" 
## Data Source: https://www.kaggle.com/groundhogclub/groundhog-day

# If you want to look at the data:
view(temps)
# What we have are yearly records of groundhog day, where each year has info about
# the temperature in Fahrenheit (for February and March, and by region in the USA), and whether or not
# the groundhog saw his shadow (NOTE: 1 - full shadow, 2 - partial shadow, 0 - no shadow)

# Some records have NA for the shadow variable, meaning we don't know what happened. 
# Let's take those empty records out: 
temps <- na.omit(temps)

# And let's filter the dataset so we are only working with years where there was a FULL shadow:
# Here are a couple ways to filter with dplyr: 
#### 1 ####
full <- filter(temps, shadow == 1)
#### 2 ####
full <- temps %>% # here we tell R what dataset we want to filter
  filter(shadow == 1)  # here we say which variable we are providing a logical for (we want the shadow variable to be equal to 1)
# note that we use %>% to tell R we are applying a function (here, filter()) to the dataframe called temps. 
# you can do it either way! 

# Now that we have a dataset with the full shadow instances, let's calculate the difference in
# temperature for those years between Februrary (overall, not regionally) and March
full$temp_diff <- full$avgtemp_march - full$avgtemp_feb
# now we can tell how much the temperature changed from Februrary to March 
# NOTE: to "call" or use a variable in a dataset, use the format dataset$VARIABLE
# the "$" indicates you're calling or referencing a variable in the dataset/dataframe 

## check on your new variable!
# use the head() function from dolyr to see if you variable creation worked: 
head(full) #shows you the first few rows and variables in the Console
view(full) #opens the dataframe in a tab for your review

# We could also change the units of original the dataset variables! 
# We just need the formula for F -> C conversion: (F − 32) × 5/9 = C
temps$feb_c <- (temps$avgtemp_feb - 32) * (5/9)
temps$march_c <- (temps$avgtemp_march - 32) * (5/9)
head(temps)

### Central Tendancy ###
# Now let's get some general information about the temperature differences (F) in years with a full shadow: 
summary(full$temp_diff)
# For years where the groundhog saw his shadow, there was a mean temperature change of 7.957 from Feb to March
# The median was 8.070, suggesting that these data might have a slight skew, but overall are pretty Normally Distributed
# The 1st quartile (25th percentile) is 5.145, 3rd quartile is 10.643 (75th percentile)
# Minimum temperature change was actually a DECREASE of 2.160, and Maximum is 20.140 degrees F

# To calculate just the quartiles of a variable: 
quantile(full$temp_diff)

# Now calculate the ***quintiles*** of variable
quantile(full$temp_diff, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)) # we fed R different percentile using the "probs" function

# Calculate the deciles 
quantile(full$temp_diff, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))

# Calculate Interquartile Range (IQR):
IQR(full$temp_diff)

### Visualizing the Data ###
## ggplot2 documentation: https://ggplot2.tidyverse.org  
# while we have an idea of the central tendency from the above calculations, 
# looking at the data is always helpful. Let's look at the distribution of temperature changes 
# using a boxplot and histogram

##### Boxplot
# helpful for visualizing the median, IQR, and other info we just calculated
# R has a built-in boxplot function: 
boxplot(full$temp_diff) # it is pretty bland and not nice to look at 

# we can try again with ggplot2: 
boxplot <- ggplot(full, aes(x=temp_diff)) + # here we are assigning the plot to a variable name
  geom_boxplot() + 
  labs(title="Boxplot of Avg. Differences in Temperature Between February and March",x="Temperature Difference (F)")
boxplot
# looks better. Don't worry about the odd display in R, when you export the plot you can change the 
# dimensions of the plot so that it has better proportions 

## ****TO EXPORT PLOT: use the "Export" button in the plot window above the plot****

# here's how to save your plot in your code: 
# 1) create an empty file (can be pdf, jpeg, etc--use ? to see other approaches)
jpeg("my_boxplot.jpeg", width = 500, height = 350) 
## if you didn't set a working directory, you will need to put your file path here. 

# 2) call your plot
boxplot

# 3) close the file you opened in step 1
dev.off()

##### Histogram
# R has a built in histogram function: 
hist(full$temp_diff) # again, it's quite plain

# let's use ggplot2 to make it a little more professional looking: 
ggplot(full, aes(x=temp_diff)) + 
  geom_histogram() # that looks a little better, but let's personalize it a bit more 

ggplot(full, aes(x=temp_diff)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey") + # changes color of bar graphs & instructs ggplot to intrepret the data frequencies as DENSITIES 
  geom_vline(aes(xintercept=mean(temp_diff)),color="blue", linetype="dashed", size=1) + # this line of code adds a marker for the mean
  geom_vline(aes(xintercept=median(temp_diff)),color="red", linetype="dashed", size=1) + # this line of code adds a marker for the median
  geom_density() + # adds the distribtion curve
  labs(title="Avgerage Temperature Change Between February and March \n in Years the Groundhog Saw his Full Shadow",x="Temperature Difference (F)", y = "Density")
  # ^^ this last line adds the titles and axis labels. Pay attention to units! 

## Make sure you save your script!!
## if you'd like, save your histogram plot using the methods above!


############ ############ ############ ############ ############ ############ 
## Extra Code / Practice  
# find the temp change (C) for all records 
temps$c_diff <- temps$march_c - temps$feb_c   

# measure the central tendacy 
summary(temps$c_diff)  

# visualize the variable (saving a plot in the process!)  
jpeg("my_histogram_celsius.jpeg")
ggplot(temps, aes(x=c_diff)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey",alpha=0.5) + # notice the inclusion of alpha. what happens if you change the value?
  geom_vline(aes(xintercept=mean(c_diff)),color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(c_diff)),color="red", linetype="dashed", size=1) +
  geom_density() + 
  labs(title="Histogram of Avg. Temperature Change Between February and March",x="Temperature Difference (C)", y = "Density")
dev.off()

# note: if you're creating a plot, the plot won't appear in the R window. 
# Check your working directory to see it! 


