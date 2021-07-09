## GGR270 Tutorial 6
# Author: Your Name
# Date: 

# This week, we are going to review hypothesis testing in R. 
# This code will use datasets provided by R, so no CSV is needed.  

# Libraries 
library(e1071)
library(RcmdrMisc)
library(tidyverse)

# Load datasets 
data("mtcars")
data("PlantGrowth")

#   ONE SAMPLE HYPO TEST PRACTICE
###############################################################################
## Mtcars ##
# This dataset contains information on fuel consumption and auto design
# obtained from the 1974 Motor Trend US magazine. 
?mtcars # this line will give you more information on the dataset

# Assume we want to know if cars have gotten lighter since 1974. 
# In 2018, the average car weight in the US was 4,000 pounds. 
# We should get some information about our data before doing a hypothesis test: 
numSummary(mtcars$wt, statistics=c("mean","sd","IQR","quantiles"),
           quantiles=c(0,.25,.5,.75,1))
# looks like the average weight in 1974 was 3,2172 pounds
# Note that our N is 32 (we have 32 observations)

# We want to test the hypothesis that the average weight of cars has *increased* since 1974. 

## Step 1: State Null Hypothesis (Ho): <<FILL IN YOUR ANSWER HERE>> 
## Step 2: State Alternative Hypothesis (Ha): <<FILL IN YOUR ANSWER HERE>> 
## Step 3. Choose a significance level (what we test our p value against): 0.05
## Step 4: Choose a statistical test. We have ONE sample with more than 30 observations.
#          With this information, we choose to do a one-sample z test (one-sided)

# Create variables you need to calculate the test statistic
xbar <- mean(mtcars$wt) # Sample Mean based on 50 observations
mu0 <- 4.0              # Hypothesized/Provided population mean 
s <- sd(mtcars$wt)      # Sample standard deviation
n <- nrow(mtcars)       # Number of observations in the sample (n > 30!!)

# Calculate the test statistic
z.stat <- (xbar-mu0)/(s/sqrt(n))
z.stat

# we could also get critical statistic in R: 
qnorm(0.05) # we use 0.05 because we have a one-sided Ha 
# -1.644854 (negative because of Ha)
## In place of using R, you could obtain critical statistic using Z table: -1.645 

# Compare test statistic to critical statistic: Can we reject null? 

# Using our *test* statistic, we can obtain a p value from a normal distribution 
# Would we use this dist. if we have 30 or less observations? 
p.val <- pnorm(z.stat, lower.tail = TRUE) 
# Why is lower.tail true? Think about our Ha! We want to obtain prob. for the tail on the LEFT
p.val # Compare p.val to the sig.level we selected above. Can we reject our Null (Ho)? 

# The average weight of cars are statistically higher today than in 1974. 

###################### New hypothesis test on mtcars dataset 
# We decide that we only want to look at automatic transmissions
# because manual transmission are rare today and might be affecting our comparison. 
auto <- filter(mtcars, am == 0)

# We should get some information about our new data before doing a hypothesis test: 
numSummary(auto$wt, statistics=c("mean","sd","IQR","quantiles"),
           quantiles=c(0,.25,.5,.75,1))
# looks like the average weight for automatic cars in 1974 was 3,769 pounds
# this average is higher than the one we calculated for all 32 cars in mtcars
# Note that our N is 19 (we have 19 observations)

# We want to test the hypothesis that the average weight of cars is different today than in 1974. 

## Step 1: State Null Hypothesis (Ho): <<FILL IN YOUR ANSWER HERE>> 
## Step 2: State Alternative Hypothesis (Ha): <<FILL IN YOUR ANSWER HERE>> 
## Step 3. Choose a significance level (what we test our p value against): 0.05
## Step 4: Choose a statistical test. We have ONE sample with LESS than 30 observations.
#          With this information, we choose to do a one-sample t test (two-sided bc of Ha)

t.test(auto$wt, mu=mu0,conf.level = 0.95, alternative="two.sided")
# df: 18
# test statistic: -1.2958; p-value: 0.2114.

qt(0.025, df=18) # must divide 0.05 by two because we're concerned with both tails (two-sided)
# crticial statistic: +-2.100922
# NOTE: qt/qnorm is *cumulative*, so it will give the critical statistic to the left of the mean
# be aware--just because qt/qnorm says negative, that does not mean the critical stat is negative
# NOTE 2: you must use qt() when doing a t.test(), NOT qnorm()
# Alternatively, you could obtain the critical statistic from t table: +- 2.101
# WHY is this crit stat +-? In the z test, we only used -

# 95% confidence interval: [3.39420, 4.14359] 
# The hypothesized mean is within the 95% CI, suggesting we might reject the null. 
# Note that in one-sided tests, one side of the CI will be 'Inf' for infinity. 

# Using the test/critical statistics and p-values, should we reject the null?

# The average weight of cars with automatic transmissions are not statistically different today than in 1974. 

#   TWO SAMPLE HYPO TEST PRACTICE - PLANT GROWTH
###############################################################################
## Plant Growth ##
# This dataset contains information on plant growth from experiments; 
# there is a control and trial groups provided (trials are *different* treatment conditions)
?PlantGrowth # this line will give you more information on the dataset

# We want to know if *trial 1* had different growth than the control group.
# We should get some information about our data before doing a hypothesis test: 
PlantGrowth %>%
  group_by(group) %>%
  # the below line is like numSummary, but within a dyplr group_by
  # note that n() provides the number of observations in that group
  summarize(mean(weight),sd(weight),IQR(weight),n())
# looks like the average growth in trial 1 was lower than the control (and sd was different)
# Note that our N is not > 30, so we can't do a Z test

# Make a simple boxplot to visually assess the data: 
boxplot(weight~group, data=PlantGrowth, xlab="Study Group", col="light green",
        ylab="Growth (dried weight of plant)", main="Your Name's Boxplot")

# We want to test the hypothesis that trial 1 has lower plant growth than the control group.
# Important: assume this experiment used twin samples to maximize validity of results. 
# Therefore, we should treat these data as PAIRED (not independent).

## Step 1: State Null Hypothesis (Ho): <<FILL IN YOUR ANSWER HERE>> 
## Step 2: State Alternative Hypothesis (Ha): <<FILL IN YOUR ANSWER HERE>> 
## Step 3. Choose a significance level (what we test our p value against): 0.05
## Step 4: Choose a statistical test. We have TWO samples, each with less than 30 observations.
#          We can do a two sample t test with a one-sided alternative hypothesis. 
#          We will NOT treat variance as equal. 

# BEFORE running the test, you MUST remove trial 2 because t.test() cannot handle more than 2 groups. 
testdf <- filter(PlantGrowth, group == "ctrl" | group=="trt1") 

t.test(weight~group, data=testdf, alternative='greater', conf.level=.95, 
       var.equal=FALSE, paired = TRUE)
# IMPORTANT: when you use the ~ to separate groups like we did above, you must verify how R reads the alternative. 
# Our Ha was that trial 1 avg. was less than control. However, R reads control first! 
# You can check this by looking at the mean of the differences (0.371) and then subtracting the means we found above. 
# Since control is considered X, we must use the alternative 'greater' 
# Please use '?t.test()' to read more about how the function deciphers the alternative. 

qt(0.05,df=9) 
# test statistic: 0.99384; critical statistic: 1.833; Can we reject Null? 
# df: 9
# p-value: 0.1731. Compare to significance level--can we reject null? 

# We have failed to reject our null. 

# The treatment in trial 1 did not have a statistically lower plant growth.

#   TWO SAMPLE PROPORTION HYPO TEST PRACTICE - mtcars
###############################################################################
?mtcars
# We want to know if there's a different proportion of automatic cars with V-shaped engines.
# We should get some information about our data before doing a hypothesis test: 
trials <- mtcars %>%
  group_by(am) %>%
  summarize(n())
trials

## Step 1: State Null Hypothesis (Ho): <<FILL IN YOUR ANSWER HERE>> 
## Step 2: State Alternative Hypothesis (Ha): <<FILL IN YOUR ANSWER HERE>> 
## Step 3. Choose a significance level (what we test our p value against): 0.05
## Step 4: Choose a statistical test. We have two samples, each with less than 30 observations.
#          We don't have measurements, but the proportion of V-shaped engines in each group. 

# get the proportion for each transmission (manual v automatic): 
successes <- mtcars %>%
  filter(vs==0) %>% # tells R to only look at V-shaped engine records
  group_by(am) %>% # splits the V-shaped records by transmission
  summarize(n())  # returns counts of V-shaped engines by transmission type 
# if the way we did it last week (splitting the dataframe into two) makes more sense,
# you can definitely do it that way as well. Whatever makes more sense to you. 
prop.test(x = successes$`n()`, n = trials$`n()`, 
          conf.level = 0.95, correct=FALSE, alternative = "two.sided")
# X-squared: 0.90688
# df = 1
# p-value: 0.3409. Compare to significance level: can we reject null (Ho)?

# In 1974, the proportion of automatic transmissions with a V-shaped engine is not 
# statistically different from manual transmissions. 








