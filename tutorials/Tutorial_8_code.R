## GGR270 Tutorial 8
# Author: Your Name
# Date: 

# This week, we are going to finish our regression example from tutorial 7.
# We will then cover spatial autocorrelation in R (simple mapping examples included)
# We are going to continue using the Toronto travel data we used last week.
# To map this data, we need a DA-level geographic file (sent with script: toronto_da_centroids)
# If you have never used a shapefile before, it is recommended you read this brief introduction: https://en.wikipedia.org/wiki/Shapefile

# Libraries: 
install.packages("sp") # if a window pops up asking for CRAN mirror, select anywhere in Canada
install.packages("classInt")
install.packages("RColorBrewer")
install.packages("spdep")
install.packages("rgdal")
install.packages("pgirmess")
library(sp)
library(classInt)
library(RColorBrewer)
library(spdep)
library(rgdal)
library(pgirmess)

# set working directory 
setwd("you/file/path")

# Load data for last tutorial's regression
da <- read.csv("transport_survey_toronto_da.csv")

## FINISHING LINEAR REGRESSION FROM TUTORIAL 7 ##
# generating our linear model that we obtained from stepwise regression in tutorial 7: 
mod2 <- lm(person_N_trips ~ Emp_notemp + hhld_vehicles + person_age + drivelic_NO, data=da)
summary(mod2)

# let's continue our analysis of mod2!!

# We should look at model diagnostic plots for Mod2 to get an idea of how well our model is performing
plot(mod2) # note: when you run this line, you will need to press ENTER in the console
# Each time you press ENTER, the plot should appear in the plot window 
# You can use the EXPORT button in the plot window to save your plots. 
# Alternatively, you can use this code to save the plots to a PDF: 
pdf("model2plots.pdf")
plot(mod2)
dev.off()

### Model Fit Plots ###
# Residuals vs Fitted #
# This plot shows us how well our model is doing (look for patterns in residuals--notice any?)
# Do the residuals look random? 

# Normal Q-Q #
# Similar to the plot we did in tutorial 7, we are looking to see
# if the residuals are normally distributed. 
# What do you think?

# Scale-Location # 
# Here, we are checking for even distribution along the range of explanatory variables. 
# Would you say these residuals are heteroskedastic?

# Residuals vs Leverage #
# This plot helps us identify outliers that may be affecting our model results. 
# Generally, you're looking for points with "leverage" (pulling the red line off it's path)
# You also want to make sure there are no points beyond Cook's Distance (the dashed red line)

# Save the residuals from your model into your original dataframe
# Must remove NAs for this to work: 
da <- na.omit(da) # NOTE: lm() automatically ignored NAs above
# Now we can add in model results:
da$residuals <- residuals(mod2)
da$fitted <- fitted(mod2)  # the values for average trips per person the model predicted 

### Now, let's MAP the observed values and residuals from our model ###
# To do this, we need spatial data to which we can append our travel data! 
# We are going to open our shapefile: 
spdf <- readOGR(dsn = ".", layer = "toronto_da_centroids") 
# note: dsn is a file path argument. We use . when our working directory is where the file is

# merge spatial data to our previous dataframe: 
?merge # make sure you look at the "Merge a Spatial* object having attributes with a data.frame"!!
df <- merge(spdf, da,by.x="DAUID",by.y="dauid",all.x=FALSE) 
# Note: because there were NULLs in our regression df, 
# all.x = FALSE so we can perform spatial autocorrelation with this dataset later. 
# moran.test() WILL NOT WORK if there are NAs; if you are just mapping, all.x = TRUE to preserve NAs

# Map Toronto DA Average trips
nclr <- 5 # Number of colors we want on our map (also used to generate classes)
plotvar <- df$person_N_trips #variable of interest
# Line below defines classes (groups) for variable of interest using fisher (see other classification types using ?classIntervals)
class <- classIntervals(plotvar, nclr, style = "fisher",dataPrecision = 2)
# Line below selects 5 colors from the color ramps 'purple to blue' or 'PuBu'
?brewer.pal # use this if you want to see other color options 
plotclr <- brewer.pal(nclr, "PuBu") 
# Using the class and the 5 picked colors, make a list of colors for each observation:
colcode <- findColours(class, plotclr, digits = 3) 
plot(df, col = colcode, axes = T,pch=16) # Displays the map we've coded above
title(main = "Average Trips per Person per Day - Your Name") # adds a title after plot is generated
legend("bottomright", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8) # add legend

## We can also map our regression model residuals:
plotvar <- df$residuals #variable of interest
# Line below defines classes (groups) for variable of interest using fisher (see other classification types using ?classIntervals)
class <- classIntervals(plotvar, nclr, style = "fisher",dataPrecision = 2)
# Line below selects 5 colors from the diverging color ramp 'PuOr'
plotclr <- brewer.pal(nclr, "PuOr") 
# Using the class and the 5 picked colors, make a list of colors for each observation:
colcode <- findColours(class, plotclr, digits = 3) 
plot(df, col = colcode, axes = T,pch=16) # Displays the map we've coded above
title(main = "Linear Model Residuals - Your Name") # adds a title after plot is generated
legend("bottomright", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8) # add legend

# Note: you could also map the predicted values of average trips per person using the 'fitted' variable

### Spatial Autocorrelation ###
# Now that we've gotten a look at our data, we can test for spatial autocorrelation

## Create Neighbor List ## 
xy <- coordinates(df)  # pulls lat lon data from our spatial dataframe
knb3 <- knn2nb(knearneigh(xy, k=3, longlat=T)) # neighbor list using 3 nearest neighbors
# note: longlat is TRUE because our coordinates are not projected (see our plot above)
# if the data are projected (e.g., the coordinates are in meters), then longlat=F
summary(knb3) # scroll to see the summary at the top! 

# we can make a neighbor list using more neighbors: 
knb4 <- knn2nb(knearneigh(xy, k=4, longlat=T))
summary(knb4)

## We can make a weights matrix using these neighbor lists: 
?nb2listw
k3_wb<-nb2listw(knb3, style="W") # creates spatial weights matrix
k3_wb

k4_wb<-nb2listw(knb4, style="W") 
k4_wb

### Moran's I ###
# once you have your weight matrix, you're able to do spatial autocorrelation tests!
k3mi <- moran.test(df$person_N_trips, listw=k3_wb, alternative="two.sided") # k=3 spatial weight
k3mi
# Our Moran's I Statistic is 0.11636. What does this tell us about our data? (clustered, dispersed, random?)
# Our p-value is very small (<0.05) What is our null hypothesis? 

# Moran Plot:
?moran.plot
moran.plot(df$person_N_trips, k3_wb,col="blue", labels=FALSE) # You can turn on labels if you want!
title(main = "Average Trips per Person per Day Moran's I Plot - Your Name") 
# Are there any outliers? Points you think are pulling the Moran's I line in a direction? 

# We can plot Moran's I into PDF
pdf("moranI_k3.pdf")
moran.plot(df$person_N_trips, k3_wb,col="blue", labels=FALSE)
title(main = "Average Trips per Person per Day Moran's I Plot - Your Name") 
dev.off()

# Another approach to Moran's I 
# You can also use what is called a Monte Carlo Markov Chain (MCMC or MC) to simulate expected values 
# MC are beyond the scope of this course, but you can see how they work below:
set.seed(100)
mc <- moran.mc(df$person_N_trips,listw=k3_wb,nsim=999) # note: alternative hypo cannot be two sided in MC Moran's I
mc # the statistic is the same
# However, our p-value is larger (but still significant)

# Additional Moran's I statistics: 
mean(mc$res[1:999]) # should be somewhat similar to Expectation from our other Moran's I test
var(mc$res[1:999]) # should be somewhat similar to Variance from our other Moran's I test
summary(mc$res[1:999])

# Plotting Monte Carlo Moran's I 
hist(mc$res, freq=TRUE, breaks=20, col="grey", xlab="Simulated Moran's I")
abline(v=0, col="red")
# you can send this plot to PDF using code provided in this script or exporting in Plot window

# Correlogram
?sp.correlogram
cor<-sp.correlogram(knb3, df$person_N_trips, order=8, method="I", style="W", zero.policy=T,  randomisation=FALSE)
print(cor)
plot(cor,ylab="Moran's I, k=3")
# how does the Moran's I statistic change over distance? Recall that the closer it gets to 0, the more random it becomes
# you might say that, as distance increases, Moran's I statistic approaches spatial randomness

# While Moran's I is the most widely used, there is another spatial autocorrelation test!

#### Geary's C ######
# 3 neighbors 
gc_k3 <- geary.test(df$person_N_trips, k3_wb, randomisation=TRUE, zero.policy=NULL,
           alternative="greater", spChk=NULL, adjust.n=TRUE)
gc_k3
# Our Geary C statistic is 0.8895. How can we interpret this? 
# Our p-value is still very small (<0.001)

# 4 neighbors
gc_k4 <- geary.test(df$person_N_trips, k4_wb, randomisation=TRUE, zero.policy=NULL,
                    alternative="greater", spChk=NULL, adjust.n=TRUE)
gc_k4
# Using more neighbors, our statistic is now 0.8959. Notice it's gotten closer to 1--why?
# Our p-value is still very small (<0.001) 

# Make sure to save your script and any plots you want to keep. 
# Next week (tutorial 9) is our final tutorial with new R code. 


#### Extra Mapping using Polygon file ####
poly <- readOGR(dsn=".",layer="toronto_da")
polydf <- merge(poly, da,by.x="DAUID",by.y="dauid",all.x=TRUE)
## Since we are not doing a spatial autocorr test, we want to keep NAs

# Similar to above: 
plotvar <- polydf$residuals #variable of interest
# Line below defines classes (groups) for variable of interest using fisher (see other classification types using ?classIntervals)
class <- classIntervals(plotvar, nclr, style = "fisher",dataPrecision = 2)
# Line below selects 5 colors from the color ramps 'purple to orange' or 'PuOr'
# We want a diverging color scheme since we have positive and negative values with a neutral (0)
plotclr <- brewer.pal(nclr, "PuOr") 
# Using the class and the 5 picked colors, make a list of colors for each observation:
colcode <- findColours(class, plotclr, digits = 3) 
plot(polydf, border = "grey",lwd = 0.002,col = colcode, axes = T) # Displays the map we've coded above
title(main = "Regression Residuals (Avg. Daily Trips)  - Your Name") # adds a title after plot is generated
legend("bottomright", legend = names(attr(colcode, "table")),fill = attr(colcode, "palette"), cex = 0.8) #add legend

