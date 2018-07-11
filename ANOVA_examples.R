#################################
#Name: Michelle R. Jackson
#Original Date: July 11, 2018
#R version: 3.5.0 "Joy in Playing"
#Purpose: Code explains how to perform a single factor ANOVA in R
#Source:http://environmentalcomputing.net/analysis-variance-single-factor/
#################################
#Analysis
Turtles <- read.csv(file = "Turtles.csv",header=TRUE) #Download Turtles file from URL above or Github.
str(Turtles) #Check that the temperature variable is a factor with the 'str' "string" function.
Turtles$Temperature <- factor(Turtles$Temperature) #There are numbers for the four levels of the Temperature treatment, we need to change the variable to a factor instead of an integer.
Turtles.aov <- aov(Days ~ Temperature, data = Turtles) #We want to create an ANOVA comparing hatching time days across temperature with the 'aov' function.
summary(Turtles.aov) #Obtain the output of the ANOVA with the 'summary' function.
Turtles.lm <- lm(Days ~ Temperature,data=Turtles) #Same analysis can be created with 'lm' function.
summary(Turtles.lm) #Obtain the output of the 'lm' function.
TukeyHSD(Turtles.aov) #Conduct a comparison between each mean and the other means by a Tukey test with the 'TukeyHSD' function.
par(mfrow = c(1,2)) #Look at assumptions following the code below. This line of code put two plots in the same window.
hist(Turtles.aov$residuals) #Look at a histogram of the residuals.
plot(Turtles.aov,which=2) #Look at QQplot of the data.
plot(Turtles.aov,which=1) #Look at homogeneity of variance 
boxplot(Days~Temperature, data=Turtles, ylab = "Hatching time (days)", xlab = "Temperature (°C)") #Create a boxplot of the results.


