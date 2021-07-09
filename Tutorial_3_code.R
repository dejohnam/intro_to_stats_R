## GGR270 Tutorial 3
# Author: Your Name
# Date: The Date 

# Load libraries 
library(dplyr)
library(ggplot2)
library(tidyverse)

# Set Working Directory 
setwd("~/Your/File/Path/Here")

# Load Data 
temps <- read_csv("groundhog_day_temps.csv")

# Calculate ***Skewness*** on February temperatures
# Here's the formula from your textbook: skewness = sum((Xi - Xbar)^3)/n*s^3
# Assuming there is no adjacent function in R, let's convert that to R code
# by breaking the equation into its parts and re-assembling: 
xbar <- mean(temps$avgtemp_feb) # xbar is the mean of the sample population
dev_cubed <- (temps$avgtemp_feb - xbar)**3 # cubed deviations from the mean
n <- length(temps$avgtemp_feb) # n is the number of observations 
s <- sd(temps$avgtemp_feb) # s is the standard deviation 
skew <- (sum(dev_cubed))/(n*(s**3)) 
skew # -0.18 indicates the data are *slightly* negative skewed
hist(temps$avgtemp_feb)

# There are skewness functions in several R libraries, let's load one: 
library(e1071) 
?skewness
skewness(temps$avgtemp_feb) 
# this approach may be easier, but it's important to know the math happening underneath!

## Let's make a quality histogram. 
feb_temp <- ggplot(temps, aes(x=avgtemp_feb)) + 
  # adding bars to histogram using frequencies/counts 
  # let's make the color of the bar correspond to the number of records in that bin!
  geom_histogram(aes(fill=..count..), color = "navy",binwidth = 1) + 
  # the line below changes the colors for our gradient. You can change the colors until you find a combo you like
  # note that the outline of the bars will remain Navy bc of the color argument in geom_hist()
  scale_fill_gradient("Count", low = "sky blue", high = "pink") + 
  # adding mean line, dotted style -- feel free to change the style
  geom_vline(aes(xintercept=mean(avgtemp_feb),color="mean"), linetype="dotted", size=1) +
  # adding median line, solid style 
  geom_vline(aes(xintercept=median(avgtemp_feb),color="median"), linetype="solid", size=1) +
  # add a simple title and axis labels
  labs(title="Histogram of Average Temperatures in February",x="Temperature (F)", y = "Count")+
  # add labels for mean and median lines and designate colors (change colors if you'd like)
  scale_color_manual(name = "statistics", values = c(median = "purple", mean = "navy"))+
  # change the theme so the background isn't grey
  # note: if you press tab after typing 'theme' you can see all the themes ggplot offers
  theme_bw()

feb_temp # If you want to see more options, check out ggplot2's documentation! 
# Note: this histogram doesn't include a density line bc it shows us *counts*/frequencies. 
# Recall that histograms can display counts of records in a bin or density of records in a bin.
# Last week's plot was a *density* histogram. Compare the ggplot code to see how we changed it! 

# another way to save plots:
ggsave("hist_feb_temps.png") # saves the last plot (use last_plot() to check)

## Notice our data seem to somewhat normally distributed.
# for practice, let's generate a random sample from a normal distribution 
# the larger we make our sample, the more it will come to resemble a normal prob. dist: 
set.seed(750)
normal <- rnorm(10000)
hist(normal)
mean(normal); median(normal) # notice how these are very close to one another! 
skewness(normal) # very small positive skew--indicating a relatively normal dist 

##################################################################
# Calculate some z-scores for years the groundhog didn't see a shadow.  
# This informs how far these temperatures were from the average. 

# filter the dataset to isolate no shadow years
noshadow <- filter(temps, shadow == 0) 

# Here's the z-score equation from your book: z = (x - xbar)/s
# let's plug our data into the equation. We already have xbar and s from above!
noshadow$zscore <- (noshadow$avgtemp_feb - xbar)/s # notice we create a new variable in the dataframe!
head(noshadow) # our z-score variables have been added to the dataframe

# let's take a look at our z-scores
noshadow$zscore

# Assume we want to focus on the most recent year in the dataset. How do we determine this? 
max(noshadow$Year) #2016 is the most recent year when the groundhog didn't see a shadow 

# let's look at the z-score for the year 2016
noshadow[noshadow$Year == 2016, ]$zscore # we didn't use the dplyr function. 
# instead, we called a subset of the dataframe using a logical statement. 
# how would we do this with dplyr? 

z2016 <- noshadow %>%
  filter(Year == 2016) %>%
  select(zscore)

# with the z-score for 2016's average February temp, interpret its meaning
z2016$zscore  
# the temp. in February 2016 was 1.73 standard deviations 
# from the average February temperature in the United States

### remember we're working with Fahrenheit! 
# If the data were in Celsius, would our standard score change? 

# let's test it: 
temps$feb_c <- (temps$avgtemp_feb - 32) * (5/9) # changes F to C

# must calc the z-scores for the original temps dataset on the Celsius data
# We are working with a new variable/unit, so our xbar and s variables aren't usable
temps$zscore <- (temps$feb_c - mean(temps$feb_c))/sd(temps$feb_c) 

# isolate the Feb. 2016 z-score: 
z2016_c <- temps %>%
  filter(Year == 2016) %>%
  select(zscore)
z2016_c$zscore  # it's exactly the same as for the Fahrenheit data. Why is this? 


# What is the probability that we observe an avg. February temperature > Feb. 2016's?
# the function pnorm provides you with the cumulative probability 
prob <- pnorm(z2016$zscore) # **is this correct?**
prob # this is actually telling you the probability of getting a score *less* than z = 1.73
# We want the probability for the area of our normal distribution that is greater than z = 1.73:
prob <- pnorm(-abs(z2016$zscore)) # How else might we calculate this? 
prob <- 1 - pnorm(z2016$zscore) # This line and the line above do the same thing. 
prob # there is a 4.21% chance of observing hotter February than we observed in 2016

## We can also find the z-score associated with a certain percentile or probability 
# for this, we use a function called qnorm -- it's in the same family as pnorm! 
# Find the z-score for the temperature at the 30th percentile: 
qnorm(0.30) # -0.52 standard deviations from the mean
# What temperature is this? 

# Calculate the temperature Celsius that corresponds to the z-score -0.52:
# We must re-organize our Z-score formula to (Z * s)+xbar = x
x <- (-0.52 * sd(temps$feb_c))+mean(temps$feb_c) 
x
# 0.05 degrees Celsius is the 30th percentile

###################################################################
## for practice, try calculating the z-scores for March temperatures. 
## Identify years where the temperature is >2 z-scores from the mean, 
## then choose 1 year and calculate the probability of observing a higher temperature
# (answer available below)
#
#
#
#
#
#
#
#
#
# one potential approach:
# 1) calculate z score
temps$march_z <- (temps$avgtemp_march - mean(temps$avgtemp_march))/sd(temps$avgtemp_march) 

# 2) select the Year, Z-score, and Temperature (F) for years with high z-scores
outlier <- temps %>%
  filter(march_z > 2 | march_z < -2) %>% # don't forget to include negative z-scores! 
  # In the above line, "|" indicates "OR", allowing us to include two logical conditions 
  select(Year,march_z,avgtemp_march)
outlier # several years had temperatures much higher or lower than the mean

# 3) Select one year and calculate the probability of observing a greater avg. temp.
score <- outlier[outlier$Year == 2012, ]$march_z
larger_prob <- pnorm(-abs(score))
larger_prob # there is a 0.16% chance of observing a higher March temperature


########################################################################
### Graphing Challenge!
# create a scatter plot of the March temperatures over time, making the 
# years identified as outliers special colors/labeled/etc (whatever you want!)
# use the ggplot2 cheat sheet (https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf) 
# or Google around for different approaches!
#
#
#
#
#
#
#
#
# one potential answer: 
library(gghighlight) # you may need to download
mean <- mean(temps$avgtemp_march)
sd <- sd(temps$avgtemp_march)
temp_over_time <- ggplot(temps, aes(Year,avgtemp_march))+ 
  geom_point(size=2,color = "red")+
  gghighlight(march_z > 2 | march_z < -2, label_key = Year, 
              unhighlighted_params = list(size = 1, color = alpha("grey", 0.4))) +
  geom_hline(aes(yintercept=mean),color="navy", linetype="solid", size=0.75) +
  geom_text(aes(x = 1890, y=mean, label="\nMean"), color="navy")+
  geom_hline(aes(yintercept=mean+sd),color="navy", linetype="dashed", size=0.5, alpha=0.5) +
  geom_hline(aes(yintercept=mean-sd),color="navy", linetype="dashed", size=0.5, alpha=0.5) +
  geom_text(aes(x = 1892, y=mean+sd, label="\n1 z score"), color="navy",alpha=0.5)+
  geom_hline(aes(yintercept=mean+(2*sd)),color="navy", linetype="dashed", size=0.5, alpha=0.5) +
  geom_hline(aes(yintercept=mean-(2*sd)),color="navy", linetype="dashed", size=0.5, alpha=0.5) +
  geom_text(aes(x = 1893, y=mean+(2*sd), label="\n2 z scores"), color="navy",alpha=0.5)+
  labs(title="Average March Temperature (1895-2016)",x="Year", y = "Temperature (F)")+
  theme_classic()
temp_over_time 
# save a plot (using ggplot2):
ggsave("outlier_temps_march.png") # saves the most recent plot


## Don't forget to save your code! Good luck on Problem Set 1!