##################################################################################
## Exploratory Data Analysis and Visualization
## Example 2
## a. data processing (continueous to categorical data conversion)
## b. violin plot
## c. stem-and-leaf plot
## d. histogram and density distribution fitting
##################################################################################

rm(list=ls(all=TRUE))

# read the data
df = read.csv('./Data/bodyfat.csv')

# -----------------------------------------------------------------------------
# 1. Read in the bodyfat.csv data file and generate a variable "bodycat" to categorize body fat into the three categories above. Make sure all 252 observations are categorized into either athlete, average, or obese.

# Athlete, less than 14%
# Average, 14-25%
# Obese, greater than 25%

df$bodycat <- 'average'
df[df$bodyfat <=14,]$bodycat <- 'athlete'
df[df$bodyfat >=25,]$bodycat <- 'obese'
df$bodycat <- factor(df$bodycat, levels=c('athlete','average','obese'))

# 2. Using summarize to identify the four height quartiles, create a new variable "htcat" to categorize height into "short", "below average", "above average", and "tall".
ht_1q = summary(df$height)[2]
ht_avg = summary(df$height)[4]
ht_3q = summary(df$height)[5]

df$htcat <- NA
df[df$height < ht_1q,]$htcat <- 'short'
df[df$height >= ht_1q & df$height < ht_avg,]$htcat <- 'below average'
df[df$height < ht_3q & df$height >= ht_avg,]$htcat <- 'above average'
df[df$height >= ht_3q,]$htcat <- 'tall'
df$htcat <- factor(df$htcat, levels=c('short','below average','above average','tall'))

# 3. Create a violin plot of weight separated by bodycat. Make sure your plots show up in some kind of order that makes sense. In complete sentences, summarize what the violin plots tell you. Are the weights evenly distributed within a range for all categories? Do athletes tend to be within a certain weight range? You may use summarize() to help you. Rough estimates are also okay.

library(lattice)

jpeg('./Figure/ex_02_violin.jpg')
bwplot(weight ~ bodycat, data = df, panel = panel.violin,
       xlab = "bodycat", ylab = "weight")
dev.off()

# 4. Create a stem-and-leaf plot for weight. Be sure to find an appropriate scale for the data.

library(multcomp)
stem(df$weight, scale=1)

# 5. (a) Create overlapping histograms of neck for the three body categories. For this exercise, do not use the default breaks. Use breaks that you think make sense. Remember to make sure that the first histogram is an appropriate window size so when you "add" the other graphs, those histograms aren't cut-off.
# (b) In the same window, add 3 density plots--1 for each body category. Do not use the default bandwidth. Use a bandwidth that you think makes sense.
# (c) In complete sentences, compare neck circumference across the three body categories using your histograms and density plots.
jpeg('./Figure/ex_02_hist.jpg')

hist(df[df$bodycat == 'athlete',]$neck, 
     col=rgb(1,0,0,0.25),
     ylim=c(0,0.35),
     breaks=seq(30,55,by=1),
     cex.axis = 0.8, 
     freq = F,
     main = "Histogram of Neck Size", 
     xlab = "Neck Size"
     )
hist(df[df$bodycat == 'average',]$neck,
     col=rgb(0,1,0,0.25),
     ylim=c(0,0.35),
     breaks=seq(30,55,by=1),
     cex.axis = 0.8, 
     freq = F,
     add = T)
hist(df[df$bodycat == 'obese',]$neck,
     col=rgb(0,0,1,0.25),
     ylim=c(0,0.35),
     breaks=seq(30,55,by=1),
     cex.axis = 0.8, 
     freq = F,
     add = T)
legend("topright", c("Athlete", "Average", "Obese"), col=c(rgb(1,0,0,0.25),rgb(0,1,0,0.25),rgb(0,0,1,0.25)), lwd=10)

dens1 <- density(df[df$bodycat == 'athlete',]$neck, bw = 0.3)
dens2 <- density(df[df$bodycat == 'average',]$neck, bw = 0.3)
dens3 <- density(df[df$bodycat == 'obese',]$neck, bw = 0.3)
lines(dens1,col=rgb(1,0,0,1),lwd=2)
lines(dens2,col=rgb(0,1,0,1),lwd=2)
lines(dens3,col=rgb(0,0,1,1),lwd=2)

dev.off()

# 6.	Repeat Question 5 for abdomen

hist(df[df$bodycat == 'athlete',]$abdomen, 
     col=rgb(1,0,0,0.25),
     ylim=c(0,0.35),
     breaks=seq(30,55,by=1),
     cex.axis = 0.8, 
     freq = F,
     main = "Histogram of Neck Size", 
     xlab = "Neck Size"
)
hist(df[df$bodycat == 'average',]$abdomen,
     col=rgb(0,1,0,0.25),
     ylim=c(0,0.35),
     breaks=seq(30,55,by=1),
     cex.axis = 0.8, 
     freq = F,
     add = T)
hist(df[df$bodycat == 'obese',]$abdomen,
     col=rgb(0,0,1,0.25),
     ylim=c(0,0.35),
     breaks=seq(30,55,by=1),
     cex.axis = 0.8, 
     freq = F,
     add = T)
legend("topright", c("Athlete", "Average", "Obese"), col=c(rgb(1,0,0,0.25),rgb(0,1,0,0.25),rgb(0,0,1,0.25)), lwd=10)

dens1 <- density(df[df$bodycat == 'athlete',]$abdomen, bw = 0.3)
dens2 <- density(df[df$bodycat == 'average',]$abdomen, bw = 0.3)
dens3 <- density(df[df$bodycat == 'obese',]$abdomen, bw = 0.3)
lines(dens1,col=rgb(1,0,0,1),lwd=2)
lines(dens2,col=rgb(0,1,0,1),lwd=2)
lines(dens3,col=rgb(0,0,1,1),lwd=2)
