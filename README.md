# intro_to_stats_R
A series of tutorials for R (data included). Starts with an introduction to R and works all the way to spatial regressions. Below is a list of the files I include here, what the learning goals are, and how to access any required data to run the lab. Data sources are credited within the code. 

#### Tutorial_1_code.R
This introductory tutorial includes the basics of R and is meant to get novice students acuqainted with coding in R Studio. They'll do simple math, assign variables, and play around with different data strucutres. No data are needed for this lab. 

#### Tutorial_2_code.R
Tutorial 2 uses the Groundhog Day temperatures dataset. The code filters the dataset, creates new variables, and explores central tendancy via boxplots, quantiles, etc. NOTE: we used the ggplot package for histograms and this was too difficult for students. I suggest using the base hist() function instead. 

#### Tutorial_3_code.R
Tutorial 3 also uses the Groundhog Day temperatures dataset. This time, students will be guided through measuring skewness, z-scores, and working with probabilities. Note that the script shows students how to break down formulas instead of using R functions (e.g., skewnewss()). 

#### Tutorial_4_code2.R
Students were provided a CSV with messy variable names to clean. I've included the messy variables in the Groundhog Day file, as well as the cleaned file that will work in the code. This tutorial guides students through simple hypothesis testing. 

#### Tutorial_5_code.R
Tutorial 5 uses a fictional dataset about two towns (metropolis and townsville). Students are guided through various hypothesis tests including t-tests (paired, unpaired, wilcoxon) and a different of proportion test. 

#### Tutorial_6_code.R
This tutorial uses datasets provided by R and reviewed hypothesis testing from tutorial 4. ANOVA was NOT included but I think this would a good place to introduce if you plan to cover that concept. 

#### Tutorial_7_code.R
Tutorial 7 covers correlation and simple linear regression. It uses data generated by UofT PhD candidate Jeff Allen (which I modified to be a smaller dataset). You could easily use a different data source and integrate into the code. 

#### Tutorial_8_code.R
Tutorial 8 continues the regression analysis that we started in tutorial 7 and concludes with spatial autocorrelation. In order to map residuals/continue the analysis, you will need Toronto DA files. These file, along with the original CSV, and included in this repository. **Please** check the code to ensure the maps are generating properly. 

#### Tutorial_9_code.R
This tutorial covers spatial regression and model selection. It continues to use the Toronto DA travel data. Please note we had some issues with the mapping code for some students, so I'd be sure to check all the mapping code for this week to ensure it works properly. 

