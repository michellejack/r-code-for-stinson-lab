#Examples of GLM's from RW MS below
#################################
#Name: Michelle R. Jackson
#Original Date: May 31, 2018
#Revised: July 19, 2018
#R version: 3.5.0 "Joy in Playing"
#Purpose: Code explains how to perform statistical analyses for the results of table 1 in Stinson et al. (2018) - in press
#Citation: Stinson, K.A., Wheeler, J.A., Record, S., and Jennings, J.L. (2018). Regional variation in timing, duration, and production of flowers by allergenic ragweed.Plant Ecology - in press. 
#################################
#Download required packages
if (!require("tinytex")) install.packages("tinytex")
if (!require("nlme")) install.packages("nlme")
if (!require("lme4")) install.packages("lme4")
if (!require("Mass")) install.packages("Mass")
if (!require("car")) install.packages("car")
if (!require("multcomp")) install.packages("multcomp")
if (!require("numDeriv")) install.packages("numDeriv")
#################################
#Analyses
# RW POPS MS Spikes & Flowering Analyses
#model revisions based on code here: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
getwd() #get working directory
setwd("/Users/michellerjackson/Documents/RW") #set working directory
library(tinytex) #load 'tinytex' package
library(nlme) #load 'nlme' package
library(lme4)#load 'lme4' package
library(MASS) #load 'MASS' package
library(car)#load 'car' package
library(multcomp)#load 'multcomp' package
library("numDeriv") #load 'numDeriv' package
ragweed<-read.csv("max_trait_data.csv") #read .csv file
head(ragweed) # show the head of the data file
str(ragweed) #show structure of the data set
na.omit(ragweed) #remove missing values
#Analysis to explore the effects of climate & site on the # of spikes - TABLE 1 ###
m1<-glmer(n_spikes~climate+site_r+(1|climate/site_r)+(1|site_r),data=ragweed, family="poisson") #model 1 with site as a random effect
#model fails to converge with 10000 evals, revise model so it runs properly
nrow(ragweed) #look at data set size (number of rows)
length(getME(m1,"theta")) #length of first model
length(fixef(m1)) #more info about length of first model
numcols <- grep("^c\\.",names(ragweed)) #"Rescale and center continuous parameters. 
#It doesn’t necessarily mess up the fit, but large differences in the scales of parameters often 
#lead to problems (especially with calculating standard deviations of fixed effects); 
#we might as well clean up this problem before we proceed."
dfs <- ragweed #revise data frame
dfs[,numcols] <- scale(dfs[,numcols]) #scale new data frame
m1_sc <- update(m1,data=ragweed) #Updated model with rescaled and continuous parameters
#"If the fit is singular or near-singular,
#there might be a higher chance of a false positive 
#(we’re not necessarily screening out gradient and 
#Hessian checking on singular directions properly); 
#a higher chance that the model has actually misconverged
#(because the optimization problem is difficult on the boundary); 
#and a reasonable argument that the random effects model should be simplified.
#The definition of singularity is that some of the constrained parameters of 
#the random effects theta parameters are on the boundary (equal to zero, or very very close to zero, say <10−6)"
tt <- getME(m1_sc,"theta") 
ll <- getME(m1_sc,"lower")
min(tt[ll==0]) 
#"Compare absolute and scaled gradient; compare gradient and Hessian with numDeriv equivalents.
#We compute the gradient and Hessian in a quick-but-less-accurate way; we can use a more precise
#(but slower) algorithm implemented by the  numDeriv package (Richardson extrapolation)."
#"Extract pre-computed information:"
derivs1 <- m1_sc@optinfo$derivs #updated model components 
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))#revised parameters
#"One general problem is that large scaled gradients are often associated with small absolute gradients:
#we might decide that we’re more interested in testing the (parallel) minimum of these two quantities:"
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
#"This is a lot smaller, although still larger than the tolerance we typically set (0.001).
#What if we redo the calculations with numDeriv?"
dd <- update(m1_sc,devFunOnly=TRUE)
pars <- unlist(getME(m1_sc,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))
#"Try restarting from previous fit … restart didn’t converge in 10000 evals, so bumped up max number of iterations."
ss <- getME(m1_sc,c("theta","fixef"))
m2 <- update(m1_sc,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4))) #revised model 
summary(m2) #revised model runs!
Anova(m2) #ANOVA of revised model
###Add. analysis exploring the effects of climate & site on spike length ###
m3<-aov(spike_length~(climate+site_r), data=ragweed) #model 3
summary(m3) #summary of model 3 ANOVA
tuk1 <- glht(m3, linfct = mcp(climate = "Tukey")) #posthoc Tukey test
summary(tuk1) #summary of Tukey
###Add.analysis exploring the effects of climate & site on crown height - TABLE 1 ###
m4<-aov(crown_height~(climate+site_r), data=ragweed) #model 4
summary(m4) #summary of model 4 ANOVA
tuk2 <- glht(m4, linfct = mcp(climate = "Tukey")) #posthoc Tukey test
summary(tuk2) #summary of Tukey
### Add. analysis exploring the effects of climate & site on inflorensence mass - TABLE 1 ###
m5<-aov(infl_mass~(climate+site_r), data=ragweed) #model 5
summary(m5) #summary of model 5 ANOVA
tuk3 <- glht(m5, linfct = mcp(climate = "Tukey")) #posthoc Tukey test
summary(tuk3)#summary of Tukey
### Add. analysis exploring the effects of climate & site on seed mass - TABLE 1 ###
morphology<-read.csv("weekly_morphology_analysis.csv") #read .csv file
na.omit(morphology) #remove NA's from 
m6<-aov(seed_mass~(climate+site_r), data=morphology) #model 6
summary(m6) #summary of model 6 ANOVA
tuk4 <- glht(m6, linfct = mcp(climate = "Tukey")) #posthoc Tukey test
summary(tuk4)#summary of Tukey 
#################################
#Name: Michelle R. Jackson
#Original Date: May 31, 2018
#Revised: July 19, 2018
#R version: 3.5.0 "Joy in Playing"
#Purpose: Code explains how to perform statistical analyses for the results of table 3 in Stinson et al. (2018) - in press
#Citation: Stinson, K.A., Wheeler, J.A., Record, S., and Jennings, J.L. (2018). Regional variation in timing, duration, and production of flowers by allergenic ragweed.Plant Ecology - in press. 
#################################
#Download required packages
if (!require("lmerTest")) install.packages("lmerTest")
if (!require("nlme")) install.packages("nlme")
if (!require("lme4")) install.packages("lme4")
if (!require("emmeans")) install.packages("emmeans")
if (!require("car")) install.packages("car")
if (!require("multcomp")) install.packages("multcomp")
if (!require("numDeriv")) install.packages("numDeriv")
#################################
#Analyses
### Repeated Measures ANOVA for # spikes over time - TABLE 3 ###
library(lme4) #load 'lme4' package
library(lmerTest) #load 'lmerTest' package
library(nlme) #load 'nlme' package
library(car) #load 'car' package
library(multcomp) #load 'multcomp' package
library(emmeans) #load 'emmeans' package
library(numDeriv) #load 'numDeriv' package
getwd() #get working directory
setwd("/Users/michellerjackson/Documents/RW") #set working directory
morphology<-read.csv("weekly_morphology_analysis_revised.csv") #read .csv file
head(morphology) #head of data table
str(morphology) #structure of data table
summary(morphology) #summary of data table
na.omit(morphology) #remove NA's
m7<-glmer(N_spikes ~ (climate*week+site_r) +(1|climate/site_r)+(1|week),data=morphology, family=poisson(link = "log"), nAGQ=0) #model 7 with site & week as a random effect
Anova(m7, type=3) #Anova of model 7
emmeans(m7, pairwise~climate*week, adjust="tukey")
### Repeated Measures ANOVA for spike length over time - TABLE 3 ###
m8<-lmer(spike_length~climate*week+site_r+(1|climate/site_r)+(1|week),data=morphology,REML=FALSE)  #model 8 with site as a random effect
anova(m8) #ANOVA for 8
emmeans(m8, pairwise~climate*week, adjust="tukey") #first attempt at Tukey test
summary(glht(m8, linfct=mcp(climate = "Tukey")), test = adjusted(type = "bonferroni")) #tukey test with Bonferroni correction
### Repeated Measures ANOVA for crown height over time - Table 3###
m9<-lmer(crown_height~climate*week+site_r+(1|climate/site_r)+(1|week),data=morphology,REML=FALSE) #model 9
anova(m9) #Anova for 9 
emmeans(m9, pairwise~climate*week, adjust="tukey") #first attempt at Tukey test
summary(glht(m9, linfct=mcp(climate = "Tukey")), test = adjusted(type = "bonferroni")) ##tukey test with Bonferroni correction
#################################
#Name: Michelle R. Jackson
#Original Date: June 1, 2018
#Revised: July 19, 2018
#R version: 3.5.0 "Joy in Playing"
#Purpose: Code explains how to perform statistical analyses for the proportion of flowers analysis for Stinson et al. (2018) - in press
#Citation: Stinson, K.A., Wheeler, J.A., Record, S., and Jennings, J.L. (2018). Regional variation in timing, duration, and production of flowers by allergenic ragweed.Plant Ecology - in press. 
#################################
#Download required packages
if (!require("tinytex")) install.packages("tinytex")
if (!require("nlme")) install.packages("nlme")
if (!require("lme4")) install.packages("lme4")
if (!require("lmerTest")) install.packages("lmerTest")
if (!require("car")) install.packages("car")
#################################
#Analyses
###Analyses of Percent Flowering###
library(lme4) #load 'lme4' package
library(nlme) #load 'nlme' package
library(tinytex) #load 'tinytex' package
library(lmerTest) #load 'lmerTest' package
library(car) #load 'car' package to run ANOVAS
getwd() #get working directory
setwd("/Users/michellerjackson/Documents/RW") #set working directory
flowers<-read.csv("percent_flowers.csv") #read .csv file
na.omit(flowers) #remove NA's
str(flowers)#structure of data frame
site_r<-flowers$site_r
indiv<-1:length(site_r)
modx<-glmer(per_flowers~climate+(1|climate/site_r),data=flowers,family="binomial") #model x for results on proportion of flowering
summary(modx) #summary of model x
Anova(modx) #ANOVA of model x
mody<-glmer(no_plants~climate+(1|climate/site_r)+(1|site_r),data=flowers,family="poisson") #model y for results on number of flowers
summary(mody) #summary of model y
Anova(mody) #ANOVA of model 

