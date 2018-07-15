#################################
#Name: Michelle R. Jackson
#Original Date: July 11, 2018
#R version: 3.5.0 "Joy in Playing"
#Purpose: Code explains how to create bar and line graphs.
#Source:http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
#################################
#Download required packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
#################################
#Practice code from 'Cookbook for R'
#load data from 'tips' dataset & create a data frame 'dat'
dat <- data.frame(
  time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(14.89, 17.23)
)
dat #show data frame
library(ggplot2) #load ggplot
# Create a very basic bar graph using ggplot
ggplot(data=dat, aes(x=time, y=total_bill)) +
  geom_bar(stat="identity")

# Map the time of day to different fill colors
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(stat="identity")

## This code have the same result as the previous lines of code: try it!
ggplot(data=dat, aes(x=time, y=total_bill)) +
  geom_bar(aes(fill=time), stat="identity")

# Add a black outline to the bars
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(colour="black", stat="identity")

# Remove legend, since the information is redundant
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE)

# Add title, narrower bars, fill color, and change axis labels
# Here the fill colors are 'Hex codes' see http://www.color-hex.com/ for examples: 
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) + 
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people")

library(reshape2) #load reshape2 package - it transforms the data between wide & long formats
# see: http://seananderson.ca/2013/10/19/reshape/
# Look at fist several rows
head(tips)
# Bar graph of counts
ggplot(data=tips, aes(x=day)) +
  geom_bar(stat="count")
## Equivalent to this, since stat="bin" is the default:
 ggplot(data=tips, aes(x=day)) +
  geom_bar()

# Basic line graph
ggplot(data=dat, aes(x=time, y=total_bill, group=1)) +
  geom_line()
## This would have the same result as above - try it!
ggplot(data=dat, aes(x=time, y=total_bill)) +
  geom_line(aes(group=1))

# Add points
ggplot(data=dat, aes(x=time, y=total_bill, group=1)) +
  geom_line() +
  geom_point()

# Change color of both line and points
# Change line type and point type, and use thicker line and larger points
# Change points to circles with white fill
ggplot(data=dat, aes(x=time, y=total_bill, group=1)) + 
  geom_line(colour="red", linetype="dashed", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white")

# Change the y-range to go from 0 to the maximum value in the total_bill column,
# and change axis labels
ggplot(data=dat, aes(x=time, y=total_bill, group=1)) +
  geom_line() +
  geom_point() +
  expand_limits(y=0) +
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people") +
  theme(plot.title = element_text(hjust = 0.5)) #this line of code centers the text

dat1 <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
)
dat1

# Stacked bar graph -- example
ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(stat="identity")

# Bar graph, time on x-axis, color fill grouped by sex -- use position_dodge()
ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")

# Change colors
ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#999999", "#E69F00"))

# Basic line graph with points
ggplot(data=dat1, aes(x=time, y=total_bill, group=sex)) +
  geom_line() +
  geom_point()

# Map sex to color
ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, colour=sex)) +
  geom_line() +
  geom_point()

# Map sex to different point shape, and use larger points
ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, shape=sex)) +
  geom_line() +
  geom_point()

# Use thicker lines and larger points, and hollow white-filled points
ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, shape=sex)) + 
  geom_line(size=1.5) + 
  geom_point(size=3, fill="white") +
  scale_shape_manual(values=c(22,21))

ggplot(data=dat1, aes(x=sex, y=total_bill, group=time, shape=time, color=time)) +
  geom_line() +
  geom_point()

# Finished bar graph
ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) + 
  geom_bar(colour="black", stat="identity",
           position=position_dodge(),
           size=.3) +                        # Thinner lines
  scale_fill_hue(name="Sex of payer") +      # Set legend title
  xlab("Time of day") + ylab("Total bill") + # Set axis labels
  ggtitle("Average bill for 2 people") +     # Set title
  theme_bw() +                               # Set background white instead of default 'grey'
  theme(plot.title = element_text(hjust = 0.5))+ #Center title
  scale_y_continuous(expand = c(0,0)) +          # Code to expand y-axis
  expand_limits(y=7)                             # Eliminates gap that comes with default graphs


# Finished line graph
ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, shape=sex, colour=sex)) + 
  geom_line(aes(linetype=sex), size=1) +     # Set linetype by sex
  geom_point(size=3, fill="white") +         # Use larger points, fill with white
  expand_limits(y=0) +                       # Set y range to include 0
  scale_colour_hue(name="Sex of payer",      # Set legend title
                   l=30)  +                  # Use darker colors (lightness=30)
  scale_shape_manual(name="Sex of payer",
                     values=c(22,21)) +      # Use points with a fill color
  scale_linetype_discrete(name="Sex of payer") +
  xlab("Time of day") + ylab("Total bill") + # Set axis labels
  ggtitle("Average bill for 2 people") +     # Set title
  theme_bw() +
  theme(legend.position=c(.7, .4))           # Position legend inside
# This must go after theme_bw

#When the variable on the x-axis is numeric, it is sometimes useful to treat it as continuous, and sometimes useful to treat it as categorical. In this data set, the dose is a numeric variable with values 0.5, 1.0, and 2.0. It might be useful to treat these values as equal categories when making a graph.
datn <- read.table(header=TRUE, text='
supp dose length
                   OJ  0.5  13.23
                   OJ  1.0  22.70
                   OJ  2.0  26.06
                   VC  0.5   7.98
                   VC  1.0  16.77
                   VC  2.0  26.14
                   ')
ggplot(data=datn, aes(x=dose, y=length, group=supp, colour=supp)) +
  geom_line() +
  geom_point()

# Copy the data frame and convert dose to a factor
datn2 <- datn
datn2$dose <- factor(datn2$dose)
ggplot(data=datn2, aes(x=dose, y=length, group=supp, colour=supp)) +
  geom_line() +
  geom_point()

# Use the original data frame, but put factor() directly in the plot specification
ggplot(data=datn, aes(x=factor(dose), y=length, group=supp, colour=supp)) +
  geom_line() +
  geom_point()

# Use datn2 from above
ggplot(data=datn2, aes(x=dose, y=length, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())

# Use the original data frame, but put factor() directly in the plot specification
ggplot(data=datn, aes(x=factor(dose), y=length, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())
