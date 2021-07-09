## GGR270 Tutorial 4
# Author: Your Name
# Date: XXX

# Load library
install.packages("abind")
install.packages("relimp")
install.packages("Rcmdr") # We need RcmdrMisc, and this line should install it and Rcmdr. 
library(abind)
library(relimp)
library(e1071)
library(RcmdrMisc)
#install.packages("RcmdrMisc") # You might need this line in order to load RcmdrMisc

# Set Working Directory 
setwd("~/Your/File/Path/Here")

# Load Data 
# *** Make sure you've reviewed the slide about cleaning data and have updated your CSV!!***
temps <- read.csv("groundhog_day_temps_v2.csv") 

# Take a look at your data
head(temps) # notice our re-named Pennsylvania variables appear at the end 

## You may be asked to standardize your data. 
## 1) Simple standardization can be centering data on 0 (generating z scores): 
temps$pa.feb.stand <- (temps$PA_feb-mean(temps$PA_feb))/sd(temps$PA_feb)

## 2) You might also want to standardize variables across a population/time. 
# You have counts of hot cocoa sold at a Tim Horton's location across 3 days.
# Your boss wants to know the standard number of hot cocoa sold per customer per day. 
# Assume there are 125 customers each day. 
set.seed(50)
cocoa <- sample.int(700,100)
standard.purchase <- cocoa/125/3 

# Get summary info about this standard purchase:
numSummary(standard.purchase, statistics=c("mean","sd","IQR","quantiles"),
           quantiles=c(0,.25,.5,.75,1))
# now we have a variable with how many hot cocoas the average customer purchased.

# You can perform standardization/math on a dataframe using the with() function: 
temps$pa.feb.stand <- with(temps, (PA_feb-mean(PA_feb))/sd(PA_feb))


#### Hypothesis Testing ####

### Z test ###
# Assume you live in Pennsylvania and you think it's warmer in PA 
# than the rest of the Northeast region. You've compiled a sample of temperatures 
# from some weather reports you had lying around and plan to test your hypothesis. 
# You saw on the news that the average feb. temperature in the NE region is 23 degree F

# Generate your sample and sample mean: 
set.seed(3)
pa <- sample(temps$PA_feb, 50) # Notice that your sample size (n) is 50!
# Get descriptive statistics about the sample
numSummary(pa, statistics=c("mean","sd","IQR","quantiles"),
           quantiles=c(0,.25,.5,.75,1))
# our mean is larger than what we heard on the news
# however, we can't say our alternative hypothesis is right. We must test it! 

## Step 1: State Null Hypothesis (Ho): 
## Step 2: State Alternative Hypothesis (Ha): 
## Step 3. Choose a significance level (what we test our p value against): 0.05
## Step 4: Choose a statistical test. We have one sample, and 50 years of observation. 
#          Because n > 30, we can do a z test. n is the size of our sample.

## One sample z-test with 95% confidence level ##
# There is not a z.test function in R/the libraries we've loaded, so we will do this by "hand"
# Use the formula in your book and lecture to find the z-test statistic:
# test stat = (xbar - mu0)/(sd/ square root of n)

# Create variables you need to calculate the test statistic
xbar <- mean(pa)# Sample Mean based on 50 observations
mu0 <- 23       # The given population mean we are testing against
s <- sd(pa)     # Sample standard deviation
n <- 50         # Number of observations in the sample (n > 30!!)

# Calculate the test statistic
z.stat <- (xbar-mu0)/(s/sqrt(n))
z.stat

# Using our test statistic, we can obtain a p value from a normal distribution 
# Would we use this dist. if we have 30 or less observations? 
p.val <- pnorm(z.stat,lower.tail = FALSE) 
#Lower tail is false because we're doing a one-sided z-test! (bc Ha is directional)
p.val # Compare p.val to the sig.level we selected above. Can we reject our Null? 

## One sample z-test with 99% confidence level (sig level 0.01) ##
# Note that our test statistic doesn't change! The equation does not rely on sig. level:
z.stat <- (xbar-mu0)/(s/sqrt(n)); z.stat

# That means what our p value also doesn't change. Why? Can we reject the null? 
p.val <- pnorm(z.stat,lower.tail = FALSE); p.val

#### t test ####
# Pennsylvania is sometimes considered to be in the Mid-West region of the US. 
# You know your state is warmer than the Northeast, but why not test the Mid-West? 
# On the evening news, the weatherperson says the avg. March temp. in the MW is 42 degrees F.  
# You compile a new sample of March temperatures for PA, but have less observations. 
set.seed(5)
pa <- sample(temps$PA_march, 23)
# Get descriptive statistics about the sample
numSummary(pa, statistics=c("mean","sd","IQR","quantiles"),
           quantiles=c(0,.25,.5,.75,1))

# You think that PA might be colder than the Mid-West, contrary to what your roommate from 
# Minnesota claims. Since your roommate is adamant it's colder in the Midwest, you decide
# to test if PA march temps are ***different*** from the Mid West (NOT colder). 

## Step 1: State Null Hypothesis (Ho): 
## Step 2: State Alternative Hypothesis (Ha): 
## Step 3. Choose a significance level: 0.05
## Step 4: Choose a statistical test. We have one sample, and 23 years of observation. 
#          Because our n <= 30, we MUST do a t test. 

## One sample t-test with 95% confidence level (two-sided) ##
# Lucky for us, there is a built-in t.test() function in R: 
t.test(pa, mu=42,conf.level = 0.95, alternative="two.sided") 
# our test statistic is -6.0391
# df is 22 (n-1)
# P-value is small--Can we reject the Null given our agreed upon significance level 0.05?


## One sample t-test with 95% confidence level (one-sided) ##
# Now you think your roommate may be wrong. 
# You decide to do a one-sided t test, your alternative being that PA march < 42
# Significance level: 95% 
# still doing a t-test (n <= 30), but one-sided this time 
t.test(pa, mu=42,alternative="less",conf.level = 0.95)
# Test statistic remains the same
# Our P value has changed. Why has our P-value gotten smaller?? 
# You can tell your roommate that Pennsylvania is colder than the Midwest! 

