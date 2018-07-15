#################################
#Name: Michelle R. Jackson
#Date: July 14, 2018
#R version: 3.5.0 "Joy in Playing"
#Purpose: Example of code for ANOVAs and figures from Stinson et al. (2018 - Ecosphere)
# GLMs (ANOVAs) investigating the effects of treatment and site on,
#soil pH, N concentrations, & moisture (Table 1)
#eradication treatment & site on earthworm biomass  (Table 2)
#mean earthworm biomass in experimental plots (Figure 1)
#################################
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("car")) install.packages("car")
if (!require("effects")) install.packages("effects")
if (!require("agricolae")) install.packages("agricolae")
if (!require("multcomp")) install.packages("multcomp")
if (!require("visreg")) install.packages("visreg")
if (!require("margins")) install.packages("margins")
if (!require("emmeans")) install.packages("emmeans")
#read csv file
#NOTE: you have to set the working directory from within Rstudio
env<-read.csv('gm_ew_env_values.csv',header=TRUE)
#display the beginning of the .csv file
head(env)
#looking at earthworm biomass as a factor of treatment & site
ewmodel1<-aov(ew~site+Treatment, data=env)
#install 'car' package to perform type II ANOVAs https://cran.r-project.org/web/packages/car/index.html
library(car)
#run type II ANOVA -- treatment has a significant effect on earthworm biomass p=0.01532
Anova(ewmodel1, type="II")
#look at the variance inflation factors of the model 
vif(ewmodel1)
#Tukey test on Treatment - invaded is different than control/marginally significantly different than eradicated
TukeyHSD(ewmodel1)
#install 'effects' package to look at treatment effects https://cran.r-project.org/web/packages/effects/index.html
library(effects) 
#plot effects of the model - RESULTS CORRESPOND WITH TABLE 2
plot(allEffects(ewmodel1)) 
#run 'agricolae' package to get additional information for tukey tests https://cran.r-project.org/web/packages/agricolae/index.html
library(agricolae)
(HSD.test(ewmodel1, "Treatment"))
#run 'multcomp' package to get additional information for tukey tests https://cran.r-project.org/web/packages/multcomp/index.html
library(multcomp)
mc = glht(ewmodel1,
          mcp(Treatment = "Tukey"))
summary(mc)
#run 'visreg' package to visualize regression https://cran.r-project.org/web/packages/visreg/index.html
library(visreg)
visreg(ewmodel1)
summary(visreg(ewmodel1))
#run 'margins' package to calculate marginal or patial effects of the model https://cran.r-project.org/web/packages/margins/index.html
library(margins)
margins(ewmodel1)
plot(margins(ewmodel1))
#run 'emmeans' package for least-squares means of the model - previously lsmeans package, but that's being phased out of R https://cran.r-project.org/web/packages/emmeans/vignettes/transition-from-lsmeans.html
library(emmeans)
#run 'ggplot2' package to create figure of the model https://cran.r-project.org/web/packages/ggplot2/README.html
library(ggplot2)
#calculate emmeans of the treatment of the model 
emmeans(ewmodel1,"Treatment")
#create data frame for the model fitted with emmeans
fig1<-summary(emmeans(ewmodel1,"Treatment"))
fig1
names(fig1)
#Data frame for the figure with code for ggplot parameters - CREATES FIGURE 1
library(ggplot2)
#Data frame for the figure with code for ggplot parameters 
myfig1<-transform(fig1,Treatment=reorder(Treatment,-emmean)) #This line makes the bars order from largest to smallest
ggplot(myfig1,aes(x = Treatment, y = emmean)) + #Establish axes
  geom_bar(position=position_dodge(), stat="identity") + #Code needed for ggplot error bars
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), #error bar code
                width=0.1, size=0.5, color="black", position=position_dodge(.9))+  labs(x = "Treatment",
                                                                                        y = "Earthworm Biomass (g)")  +
  theme_bw()+ #set white background 
  theme(axis.text = element_text(size=12,colour = "black"))+  #set both axes size
  theme(axis.title.x = element_text(size=12,face="bold")) + #set x-axis text
  theme(axis.title.y = element_text(size=12,face="bold")) + #set y-axis test
  theme(legend.title = element_text(size=12, face="bold"))+ #set legend title, but we're exlcuding the legend below
  theme(legend.text = element_text(size = 12))+  #set legend text
  scale_y_continuous(expand = c(0,0)) + #expand y-axis to remove gap
  expand_limits(y=7) + #code for gap removal
  theme(legend.position = "none")+ #remove lgend
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black")) #set border/other axis details
############################# Exporting the plot #######################################
#to export graph repaste code needed for figure
figure1<-ggplot(myfig1,aes(x = Treatment, y = emmean)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE),
                width=0.1, size=0.5, color="black", position=position_dodge(.9))+  labs(x = "Treatment",
                                                                                        y = "Earthworm Biomass (g)")  +
  theme_bw()+
  theme(axis.text = element_text(size=12,colour = "black"))+
  theme(axis.title.x = element_text(size=12,face="bold")) +
  theme(axis.title.y = element_text(size=12,face="bold")) +
  theme(legend.title = element_text(size=12, face="bold"))+
  theme(legend.text = element_text(size = 12))+
  scale_y_continuous(expand = c(0,0)) +
  expand_limits(y=7) +
  theme(legend.position = "none")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))

ggsave("ew_fig1_resize.tiff", figure1, units="in", width=5, height=5, dpi=300) #name of file & size of figure
tiff("ew_fig1_resize.tiff", width=5, height=5) #turn figure into a tiff
ggplot(myfig1,aes(x = Treatment, y = emmean)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE),
                width=0.1, size=0.5, color="black", position=position_dodge(.9))+  labs(x = "Treatment",
                                                                                        y = "Earthworm Biomass (g)")  +
  theme_bw()+
  theme(axis.text = element_text(size=12,colour = "black"))+
  theme(axis.title.x = element_text(size=12,face="bold")) +
  theme(axis.title.y = element_text(size=12,face="bold")) +
  theme(legend.title = element_text(size=12, face="bold"))+
  theme(legend.text = element_text(size = 12))+
  scale_y_continuous(expand = c(0,0)) +
  expand_limits(y=7) +
  theme(legend.position = "none")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
