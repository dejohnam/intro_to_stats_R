## GGR270 Tutorial 5
# Author: Your Name
# Date: XXX

# Libraries 
library(e1071)
library(RcmdrMisc)
library(dplyr)

# Set working directory 
setwd("your/file/path/here")

# Load Data 
data <- read.csv("neighborhood_data.csv")

# look at data
head(data)

### Two-Sample t Test ###
# We want to compare the cities, starting with income
# Average income, metropolis
metropolis <- data %>%
  filter(city == "metropolis")
numSummary(metropolis$avg_income, statistics=c("mean","sd","IQR","quantiles"),
           quantiles=c(0,.25,.5,.75,1))
hist(metropolis$avg_income,breaks = 6, probability = TRUE)
densityPlot(metropolis$avg_income)

# Average income, townville
townville <- data %>%
  filter(city == "townville")
numSummary(townville$avg_income, statistics=c("mean","sd","IQR","quantiles"),
           quantiles=c(0,.25,.5,.75,1))
hist(townville$avg_income, breaks=6, probability=FALSE)
densityPlot(townville$avg_income)

## Boxplot comparing two cities avg. income
boxplot(avg_income~city, data=data, xlab="City",
        ylab="Mean Household income (USD)", main="Your Name's Boxplot")

# We might hypothesize that metropolis's average household income is lower than townville's
# because the boxplot for metropolis is sitting below townville's. 
# Our hypothesis test will show that these two cities are not equal.

## Step 1: State Null Hypothesis (Ho): 
## Step 2: State Alternative Hypothesis (Ha): 
## Step 3. Choose a significance level (what we test our p value against): 0.05
## Step 4: Choose a statistical test. We have two samples, each with less than 30 observations.
#          N < 30 & Normally Distributed data means we can do a two-sample t-test
#          Note that the variance isn't exactly equal, we will assume it is. 

t.test(avg_income~city, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=data)
# test statistic: -9.6732
# df: 31
# p-value: 7.043e-11. Compare to significance level--can we reject null? 
# The average household income in metropolis significantly different from townville


### Paired Two-Sample t test ###
# This time, we want to see if the number of doctor's visits per capita went up in metropolis
# between 2010 and 2019 even though the percent insured remained the same 
numSummary(metropolis$doc_visits2010, statistics=c("mean","sd","IQR","quantiles"),
           quantiles=c(0,.25,.5,.75,1))
numSummary(metropolis$doc_visits2019, statistics=c("mean","sd","IQR","quantiles"),
           quantiles=c(0,.25,.5,.75,1))
boxplot(metropolis$doc_visits2010, metropolis$doc_visits2019, names = c("2010","2019"), 
        xlab="Year of Observation", ylab="Per Capita Doctor's Visits", main="Your Name's Boxplot")

# We might hypothesize that metropolis's per capita doctor's visits is going up 
# because the boxplot for 2019 is skewed higher and has a higher mean
# Our hypothesis test will show that there are more doctor's visits per capita in 2019. 

## Step 1: State Null Hypothesis (Ho): 
## Step 2: State Alternative Hypothesis (Ha): 
## Step 3. Choose a significance level (what we test our p value against): 0.05
## Step 4: Choose a statistical test. We have two samples, each with less than 30 observations.
#          The re-observation of the same neighborhoods in two years means the samples are dependent.

t.test(metropolis$doc_visits2010, metropolis$doc_visits2019, 
       alternative='less', conf.level=.95, paired=TRUE)
# test statistic: -3.3556
# df: 14
# p-value: 0.002355 Compare to significance level 0.05--can we reject null? 
# The per capita doctor's visits were significantly higher in metropolis in 2019 compared to 2010. 


### Wilcoxon t test ###
# We want to know if the amount of uninsured individuals is higher in metropolis than townville
hist(metropolis$uninsured, breaks=5, col="blue",
     xlab ="Count Without Health Insurance",
     main="Your Names Histogram of metropolis Uninsured")
hist(townville$uninsured, breaks=5, col="red",
     xlab ="Count Without Health Insurance",
     main="Your Names Histogram of townville Uninsured")
boxplot(uninsured~city, id.method="none", data=data,
        xlab="City", ylab="Count without Insurance in a Neighborhood", main="Your Name's Boxplot")

# We might hypothesize that metropolis's uninsured population is different than townville's
# Our hypothesis test will show that these two cities are not equal. 

## Step 1: State Null Hypothesis (Ho): 
## Step 2: State Alternative Hypothesis (Ha): 
## Step 3. Choose a significance level (what we test our p value against): 0.01
## Step 4: Choose a statistical test. We have two samples, each with less than 30 observations.
#          The data do not look normally distributed.We assume the samples are independent. 

wilcox.test(uninsured ~ city, data = data, exact = FALSE)
# W = 74.5
# p - value: 0.03003. Can we reject the null given our significance level? 
# There is not a significant difference in the amount of uninsured people in metropolis and townville. 


### Difference in Proportions ### 
# We want to know which city has a higher proportion of social disorder (scores >= 4)
hist(metropolis$social_disorder, breaks=5, col="blue",
     xlab ="Social Disorder Score",
     main="Your Names Histogram of metropolis Social Disorder Scores")
hist(townville$social_disorder, breaks=5, col="red",
     xlab ="Social Disorder Score",
     main="Your Names Histogram of townville Social Disorder Scores")

# We might hypothesize that metropolis's social disorder is higher than townville's
# Our hypothesis test will show that metropolis has more social disorder than townville. 

## Step 1: State Null Hypothesis (Ho): 
## Step 2: State Alternative Hypothesis (Ha): 
## Step 3. Choose a significance level (what we test our p value against): 0.05
## Step 4: Choose a statistical test. We have two samples, each with less than 30 observations.
#          We don't have measurements, but the proportion of social disorder in each city. 

# get the proportion for each city: 
successes <- c(nrow(filter(metropolis,social_disorder>=4)),
               nrow(filter(townville,social_disorder>=4)))
trials <- c(nrow(metropolis),nrow(townville)) # number of observations in metropolis and townville

prop.test(x = successes, n = trials, conf.level = 0.95, correct=FALSE, alternative = "greater")
# x-squared: 1.3398
# df = 1
# p-value: 0.1235. Can we reject Null??
# The proportion of metropolis neighborhoods with social disorder is not higher than townville's. 
