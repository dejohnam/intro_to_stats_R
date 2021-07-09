## GGR270 Tutorial 9 
# Author: Your Name
# Date: 

# This week, we are conducting spatial regressions.
# We are going to continue working with the Toronto travel data. 

# Libraries: 
## Install packages
install.packages("maps")
install.packages("maptools")
install.packages("sp")
install.packages("spdep")
install.packages("pgirmess")
install.packages("RColorBrewer")
install.packages("classInt")
install.packages("lmtest")
install.packages("car")
install.packages("spatialreg")
install.packages("spData")

library(maps)         # Projections
library(maptools)     # Data management
library(sp)           # Data management
library(rgdal)        # Data Management
library(spdep)        # Spatial autocorrelation
library(pgirmess)     # Spatial autocorrelation
library(RColorBrewer) # Visualization
library(classInt)     # Class intervals
library(lmtest)       # Linear regression diagnostic
library(car)          # Applied regression diagnostic
library(spData)       # required for spatialreg package
library(spatialreg)   # Spatial regression


# set working directory 
setwd("Your/File/Path/Here")

## Load datasets ##
# Travel Data
da <- read.csv("transport_survey_toronto_da.csv")
da <- na.omit(da) # removing NAs now before merging
# Toronto DA centroids
spda <- readOGR(dsn = ".", layer = "toronto_da") 
# merged files
df <- merge(spda, da,by.x="DAUID",by.y="dauid",all.x=FALSE) 

# Create matrix of polygon centroids
xy <-SpatialPoints(df[,c("x","y")]) # Creates SpatialPoints dataframe from XY coordinates in polygon file
summary(xy)
df_k2 <- knn2nb(knearneigh(xy, k=2, longlat=T)) # using 2 nearest neighbors
df_k2_wb <- nb2listw(df_k2, style="W") # creating weight matrix from neighbor list
df_k2_wb

# Our *final* OLS regression from tutorial 7 & 8 
# Recall this model predicts average trips per person per day in a neighborhood:
ols <- lm(person_N_trips ~ Emp_notemp + hhld_vehicles + person_age + drivelic_NO, data=df)
summary(ols)

# This time, let's analyze the spatial autocorrelation in the residuals using Moran's I:
lm.morantest(ols, df_k2_wb) # notice we use the model and weights matrix
# The moran's I statistic is 0.10214 (slight clustering in residuals)
# The p-value is smaller than 0.05, meaning it is statistically significant

# We can calculate our Moran's I for our dependent var again (more on this in tut 8):
moran.test(df$person_N_trips, listw=df_k2_wb, alternative="two.sided") # using k = 2 this time
# Moran's I is 0.11968 and p < 0.05 

# Now, let's use our Lagrange Multiplier to test for spatial dependence in our residuals (errors)
lm.LMtests(ols, df_k2_wb, test="all") 
# again, using 2 neighbors--you could change to 3 or 4 in our code above and redo this to see how it changes
# This test gives us 4 different values, but they are all telling us similar information: 
# LMerr is the Lagrange Multiplier for error (43.931) and it is statistically sig.
# LMlag is the Lagrange Multiplier for lag (46.556) and it is statistically sig.
# RLMerr & RLMlag are simply robust estimates of error and lag -- (0.089, 2.7143)
# Neither of the robust estimations are statistically significant, but RLMlag is approaching signficance at the 0.05 level
# In general, you want these statistics to be higher (the higher the better for the validity of the adjacent model)

# Given these results, we might prefer the lagged model, as it appears slightly more significant.
# We will run both for practice:

# Spatial Error Model: (THESE MODELS TAKE A LONG TIME TO RUN, SO DO NOT RUN UNLESS YOU HAVE A SEVERAL MINUTES)
#df.err<-errorsarlm(person_N_trips ~ Emp_notemp + hhld_vehicles + person_age + drivelic_NO, data=df, listw=df_k2_wb, tol.solve=1e-20)
#summary(df.err)
# ** we don't have time to run each model in tutorial. The model summary is provided in the CSV below:
errmod <- read.delim("error_model_sum.txt")
errmod # this line returns the same information available in summary(df.err)
# All variables are statistically significant. 
# Notice that the Beta values (Estimates) are largely the same, although some have increased (meaning the relationship is larger than in the OLS)
# We are also provided with a LAMBDA, which is our spatial term, it's somewhat small (close to 0) and statistically sig

# Spatial Lag Model:  (THESE MODELS TAKE A LONG TIME TO RUN, SO DO NOT RUN UNLESS YOU HAVE A SEVERAL MINUTES)
#df.lag<-lagsarlm(person_N_trips ~ Emp_notemp + hhld_vehicles + person_age + drivelic_NO, data=df, listw=df_k2_wb, tol.solve=1e-20) 
#summary(df.lag)
# ** we don't have time to run each model in tutorial. The model summaries are provided in the CSV below:
lagmod <- read.delim("lag_model_sum.txt")
lagmod # this line returns the same information available in summary(df.lag)
# All variables are statistically significant here as well
# Notice that the Beta values (Estimates) are largely the same as in the OLS
# We are also provided with a RHO, which is our spatial lag term, it's somewhat small (close to 0) and statistically sig

# Now that we've seen our model results, we should choose between the two models. 
# Neither model had particularly promising results, in fact both were very similar to our OLS,
# likely because our Moran's I tests show small levels of spatial autocorrelation. 
# As a researcher, it's your job to decide on a model. 
# Since both models had sig. spatial terms and sig. LMtest results, we will use the model
# that our LMtest suggested was more statistically significant: lagged model.

# Save fitted values and residuals into dataset
# Load the line below if in tutorial--otherwise continue as usual with the commented lines below:
df <- read.csv("toronto_data_model_output.csv")
#df$err.residuals <- residuals(df.err)
#df$err.fitted <- fitted(df.err)
#df$lag.residuals <- residuals(df.lag) 
#df$lag.fitted <- fitted(df.lag)

#Moran test on residuals (only looking at lagged model since it's what we selected)
moran.test(df$lag.residuals, listw=df_k2_wb, alternative="two.sided")
# Moran's I statistic is very close to zero and non-significant -- this indicates the data are random!
# In other words, we've failed to reject our Null that the residuals are spatially random

## Model diagnostics ##
# Studentized Breusch-Pagan test (only run if working on your own)
#bptest.sarlm(df.lag) 
# tests whether the residual variance is dependent on independent variables

# Calculate pseudo r-squared 
pr2_sm <-(cor(df$person_N_trips,df$lag.fitted))^2
pr2_sm # 8.42% of variance in person_N_trips is described by our model
# While still small, this is higher than the r^2 we observed in our OLS (6.74%)

# Visualize the residuals with a histogram
hist(df$lag.residuals, breaks=8, col= "grey",
     main="Distribution of lagged model residuals - Your Name",xlab="residuals")
# Please do not forget to add your name! You will be penalized on the PS if you don't! 
# Note that residuals appear normally distributed

#Normal Q-Q plot
qqnorm(df$lag.residuals, main="My Name's Q-Q Norm Plot of Residuals")
qqline(df$lag.residuals, col="red")
# Do we think our residuals are normal? 

#Fitted vs. Residuals plot
plot(df$lag.fitted, df$lag.residuals, main="My Name's Plot of Predicted Values and Residuals")
abline(h=0, col="red")
identify(df$lag.fitted,df$lag.residuals, labels=row.names(df))
# with your pointer, click on desired points (like outliers) for id #. 
# Press ESC when you've clicked all desired points, and then the IDs will appear on the plot!
# Do the residuals look random to you?

# You can export fitted values and residuals into DBF file
install.packages("foreign")
library("foreign")
write.dbf(df, "SAR_values.dbf", max_nchar = 254)

## Final thoughts on this example: 
# When we were treating our data a-spatially (in the OLS), 
# there were some signs of spatial autocorrelation in our Moran's I tests.
# Using the Lagrange Multiplier, we decided a spatial model might help increase the validity of our model. 
# We built an error and lagged model, and compared results. Both were valid, 
# so we selected the lagged model based on the LM test results. 
# The spatially lagged model had a higher R-squared, 
# meaning the spatial model explains more of the variance in our dependent var (person_N_trips)
# The residuals of the spatial model are spatially random and appear to be normally distributed. 

## Extra Mapping Practice ##
# If in tutorial, you **MUST** merge the CSV data we loaded with the spatial dataframe:
df <- merge(spda, df,all.x=TRUE) # you **shouldn't** run this line if you ran the model on your own!!
# Mapping residuals from Spatial Lag Model
nclr <- 5
plotvar <- df$lag.residuals #variable of interest
# Line below defines classes (groups) for variable of interest using fisher (see other classification types using ?classIntervals)
class <- classIntervals(plotvar, nclr, style = "fisher",dataPrecision = 2)
# Line below selects 5 colors from the color ramps 'purple to orange' or 'PuOr'
# We want a diverging color scheme since we have positive and negative values with a neutral (0)
plotclr <- brewer.pal(nclr, "PuOr") 
# Using the class and the 5 picked colors, make a list of colors for each observation:
colcode <- findColours(class, plotclr, digits = 3) 
plot(df, border = "grey",lwd = 0.002,col = colcode, axes = TRUE) # Displays the map we've coded above
title(main = "Spatially Lagged Residuals (Avg. Daily Trips)  - Your Name") # adds a title after plot is generated
legend("bottomright", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8) #add legend

## Compare this map to the one at the end of tutorial 8 to see any changes! 
