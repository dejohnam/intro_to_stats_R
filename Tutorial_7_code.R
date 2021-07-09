## GGR270 Tutorial 7
# Author: Your Name
# Date: 

# This week, we are going to cover correlation and simple linear regression.
# This code references a subset of data created by UofT PhD candidate Jeff Allen. 
# See Jeff's publication using the data here: https://www.sciencedirect.com/science/article/pii/S1361920919308788?via%3Dihub
# It contains DA-level estimates of transportation info like trips per person, transit passes, etc.  

# Load CSV 
da <- read.csv("transport_survey_toronto_da.csv")

# Assume that we want to know what neighborhood characteristics (e.g., avg. employed, avg. age, etc)
# explain variation in the number of avg. trips 

# Data Exploration
## Normal Q-Q Plot 
qqnorm(da$person_N_trips,
       main="Normal Q-Q Plot of Mean Trips per Person - Your Name",
       xlab="Theoretical Quantiles of Mean Trips per Person",
       ylab="Sample Quantiles of Mean Trips per Person")
qqline(da$person_N_trips, col="red")

# What do you notice about the data? Do you think these data are from a theoretical normal distribution? 

# Let's do another Normal Q-Q plot of a different variable in the dataset: 
qqnorm(da$drivelic_NO,
       main="Normal Q-Q Plot of No License - Your Name",
       xlab="Theoretical Quantiles of # No License in Dissemination Area",
       ylab="Sample Quantiles of # No License in Dissemination Area")
qqline(da$drivelic_NO, col="red")

# What have you noticed has changed? Which variable would you say is more normally distributed?


# We can create a correlation matrix of all variables: 
corrmatrix <- cor(da[2:13], use = "complete.obs", method="pearson") # we must use [2:13] to leave off DA id variable
corrmatrix
round(corrmatrix, 2) # this line rounds the correlations so it's easier to read
# Does anything look highly correlated? Recall correlation coefficients go from -1 to 1
# Notice that you can not only see what is correlated to the dependent variable (person_N_trips),
# but you can also see how the other variables are correlated to one another. 
# Notice that drivelic_NO and drivelic_YES are highly correlation (0.67) because they're 
# explaining the same thing. Both of these variables are also highly correlated with TPass_No (0.82,0.91).

# Save the correlation matrix into excel
write.csv(corrmatrix, "da_corr_matrix.csv")

# You can do individual correlations if you specify variables: 
cor(da$person_N_trips,da$drivelic_NO, use = "complete.obs", method="pearson")
# so we can see that estimated mean trips per person and estimated number of folks without a 
# driver's license have a weak negative correlation. 

# To visualize our correlation, we can make a scatter plot: 
plot(da$drivelic_NO,da$person_N_trips, main="Your Name's Scatterplot", 
     xlab="Estimated # Without Driver's License", ylab="Mean # Trips per Person")
abline(lm(da$person_N_trips~da$drivelic_NO), col="red") 
# line of best fit between the two variables ^^
# Looks like there are potentially a few DAs with leverage

# We could also look at relationships between independent variables with our scatterplot: 
plot(da$drivelic_YES,da$TPass_No, main="Your Name's Scatterplot", 
     xlab="# With License", ylab="# Without Transit Pass")
abline(lm(da$TPass_No~da$drivelic_YES), col="red") 
# Very clear linear relationship--this is reflective of the high r coeff. (0.91)


# If you want to know how significant a correlation is, you can use a different 
# correlation function that will provide a p-value: 
cor.test(da$person_N_trips,da$drivelic_NO, use = "complete.obs", method="pearson")
# Here we can see that although the correlation may be considered weak,
# it is highly significant (p < 0.001)

# you can also do a simple linear regression between two variables to estimate their 
# relationship without controlling for any confounding variables: 
mod0 <- lm(da$person_N_trips~da$drivelic_NO)
# note: whatever comes before the ~ is the *dependent* variable. Variable after ~ is *independent*

# view the results of the model: 
summary(mod0) 
# statistically significant correlation often leads to significance within a regression
# read through the output--what do you recognize? 
# *Estimate* is the Beta value--this is what we use to describe one variable's effect on the other
# We would say "for every 1 unit increase in no driver's license, we would expect
# the average number of daily trips per person to decrease .0004847" (or increase -.0004847)
# We can also see this is a statistically significant relationship using the p-value (Pr(>|t|))
# Look to the bottom of the summary--here we find r^2. 
# We might say "2.792% of the variation in the average number of daily trips per person
# can be explained by the number of people with no driver's license"
# Note that the y-intercept (b in the y=ax+b equation) is provided as (Intercept) in the model output. 


### Step-wise Regression Model of Avg. Trips per Person ####
# Now that we know a bit about our data and have gone through a simple linear regression output,
# let's build a model with multiple independent/explanatory variables. 
# Use all variables, even weakly correlated ones (we will use a cutoff of +-0.1 to save time)
# Note: we are only looking at what is correlated with our dependent variable! 
mod1 <- lm(person_N_trips ~ Emp_notemp + stu_fulltime +
           hhld_vehicles + person_age + drivelic_NO, data=da)
# whatever comes before the ~ is the *dependent* variable. Variables after ~ are independent

# view the results of the model: 
summary(mod1)
# Notice we have one variable that is not significant (stu_fulltime)
# We we will remove and re-do the model

mod2 <- lm(person_N_trips ~ Emp_notemp + hhld_vehicles + person_age + drivelic_NO, data=da)
summary(mod2)
# notice how the Unemployed variable (which had a p-value of 0.012 in mod1) is now more significant! 
# If there were a variable that wasn't significant, we would remove and make another model.
# We would repeat this step-by-step process until all variables in the model were significant. 
# Note that the regression estimates (Beta, P-val, r^2) will change between each model because 
# we are estimating a new regression line each time, changing the fit of the model! 

# Note that in mod2, the Multiple R^2 is 0.06837 (adj. 0.06735). Both of these are quite low!
# Let's remove the least significant variable just to see the effect on R^2

mod3 <- lm(person_N_trips ~ Emp_notemp + hhld_vehicles + person_age, data=da)
summary(mod3)
# Notice that R^2 has decreased, meaning without drivelic_no, our model explains less variation
# in the mean number of trips per person. 
# Mod2 is the preferred model because it explains more! 

summary(mod2)
# With this model, we can say that there is a statistically significant, *negative* relationship 
# between the average number of trips per person per day and
# the number of people unemployed, the average age, and the number of people with no license in a DA. 
# We also observed a statistically sig. *negative* relationship between the average number of vehicles per hhld
# and the average number of trips per person per day. 

# When controlling for unemployment, household vehicles, and age, we observed that as the # of people 
# without a drivers license increases by 1, the average of daily trips per person will decrease .00029.


## That's it for this week, but we will be working more with these data in tutorial 8. ##

