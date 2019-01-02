####################################
### Project 2                    ###
### MSBC 5030                    ###
### Team 2                       ###
### Neven Grgic, Sarah Wofford,  ###
### Emily Williams, Shian Zheng, ###
### Aditi Tripathi               ###
### 10/16/18                     ###
####################################

sink("Project 2 log", append=FALSE, split=TRUE) 

#set working directory and read in data#
setwd( 'C:/Users/ngrgi/OneDrive/Documents/MS Courses/Quantitative Methods')
Invest <- read.csv("GCE.csv", stringsAsFactors = FALSE)
Unrate <- read.csv("UNRATE.csv", stringsAsFactors = FALSE)
GDP <- read.csv("GDP.csv", stringsAsFactors = FALSE) 

###############################
### Q1: Produce the Dataset ###
###############################

#Reformat date for unemployment data and pull out month and year
Unrate$date <- as.Date(Unrate$DATE, format = '%m/%d/%Y')
Unrate$month <- as.numeric(substr(Unrate$date, start = 6, stop = 7))
Unrate$year <- as.numeric(substr(Unrate$date, start = 1, stop = 4))

#trim data to be quarterly using month and add Investment and GDP data starting in 1948
#Rename this Quarterly Data
QuarterlyData <- Unrate[Unrate$month == 1 | Unrate$month == 4 | Unrate$month == 7 |
                          Unrate$month == 10, c("date", "month","year", "UNRATE") ]
QuarterlyData$Investment <- Invest$GCE[5:287]
QuarterlyData$GDP <- GDP$GDP[5:287]

#add variable for investment as a percentage of GDP
QuarterlyData$Percentage <- (QuarterlyData$Investment/QuarterlyData$GDP) *100

#Remove last row of dataframe since Q3 2018 is not yet reported for Investment and GDP
QuarterlyData <- QuarterlyData[-nrow(QuarterlyData), ]

#Include only years 1990-2010 to replicate Taylor's plot
Plot1QuarterlyData <- QuarterlyData[QuarterlyData$year >= 1990 & QuarterlyData$year <= 2010, ]

#Trim to not include Q4 2010, as Taylors final data point is Q3 2010
#Q1DF provides the data needed to recreate Taylor's other plot
Q1DF <- Plot1QuarterlyData[-nrow(Plot1QuarterlyData), ]

##########################
###  Q2 Make the plot  ###
##########################

#square off the x and y axis to replicate Taylor's graph
par(pty = "s")

plot(Q1DF$Percentage, Q1DF$UNRATE, xlim = c(17.5,21.5), ylim = c(3,11),
     ylab = "Unemployment Rate", xlab = "Government purchases as a percentage of GDP",
     xaxt = "n", yaxt = 'n', type = "p", col = "blue", pch = 19,
     main = "Taylor's Other Plot")
xticks <- seq(from = 17.5, to = 21.5, by = .5)
yticks <- c(3:11)
axis(1, at = xticks, labels=xticks)
axis(2, at = yticks, labels = yticks)

###################################
### Q3: plot the complete data  ###
###################################

#Make dataframes for before and after taylors data
BeforeTaylorQuarterlyData <- QuarterlyData[QuarterlyData$year < 1990, ]
AfterTaylorQuarterlyData <- QuarterlyData[QuarterlyData$year >= 2010, ]
#Cut Q1-Q3 of 2010 out of the After Data
AfterTaylorQuarterlyData <- AfterTaylorQuarterlyData[4:34, ]

#Make a plot that includes all data from 1948 to 2018. First, add Taylor's data
par(pty = "s")
plot(Q1DF$Percentage, Q1DF$UNRATE, xlim = c(15,25), ylim = c(2,11),
     ylab = "Unemployment Rate", xlab = "Government purchases as a percentage of GDP",
     xaxt = "n", yaxt = 'n', type = "p", col = "blue", pch = 19)
xticks <- seq(from = 15, to = 25, by = .5)
yticks <- c(2:11)
axis(1, at = xticks, labels=xticks)
axis(2, at = yticks, labels = yticks)

#add point for before and after the current plot
points(BeforeTaylorQuarterlyData$Percentage, BeforeTaylorQuarterlyData$UNRATE, type = 'p', col = 'red', bg = 'red', pch = 22)
points(AfterTaylorQuarterlyData$Percentage, AfterTaylorQuarterlyData$UNRATE, col = 'green', bg = 'green', pch = 24)
legend(15, 11, legend=c("Q1 1948 - Q4 1989", "Q1 1990 - Q3 2010", "Q4 2010 - Q2 2018"),
       col=c("red", "blue", "green"), pch = c(22, 19, 24), pt.bg = c("red", "blue", "green"), cex = .55)

###################################
### Q4: Analyze correlations    ###
###################################

#Calculate correlation from Taylor's sample
cor.test(Q1DF$UNRATE, Q1DF$Percentage)

#Calculate correlation from the entire data set (1948-2018)
cor.test(QuarterlyData$UNRATE, QuarterlyData$Percentage)

#Correlation from only before Taylor's dataset (1948- 1989)
cor.test(BeforeTaylorQuarterlyData$UNRATE, BeforeTaylorQuarterlyData$Percentage)

#Correlation from only after Taylor's dataset (Q42010 - Q22018)
cor.test(AfterTaylorQuarterlyData$UNRATE, AfterTaylorQuarterlyData$Percentage)

########E########################################################################
##### Plot a graph of correlations for the prior 10 years from each quarter #####
#################################################################################

#Initialize empty vector to hold correlations for prior ten years of data from each datapoint
QuarterlyData$tenYearCor <- rep(NA, times = nrow(QuarterlyData))

#calculate correlation for the past decade for each datapoint and store in the vector we created
for (i in 40:nrow(QuarterlyData)){
   QuarterlyData$tenYearCor[i] <- cor(QuarterlyData$UNRATE[(i-39):i], QuarterlyData$Percentage[(i-39):i])
}

#create date variable for plotting to show the date where data starts for calculation of 
#correlation at each point. Since we are going back ten years, date - 10 years
dateLessTen <- as.POSIXlt(QuarterlyData$date)
dateLessTen$year <- dateLessTen$year - 10
QuarterlyData$dateLessTen <- as.Date(dateLessTen)

#plot the correlations for the past 10 years at each point
par(pty = 'm')
plot(QuarterlyData$dateLessTen, QuarterlyData$tenYearCor, type = 'l', 
     xlim = c(QuarterlyData$date[1],QuarterlyData$date[nrow(QuarterlyData) - 40]),
     ylim = c(-1,1),
     xlab = "Starting Date of Sample", ylab = "Correlation",
     main = " Correlation Between Unemployment Rate and Government Expenditures for prior 10 years",
     col = "blue")
abline(v = QuarterlyData$date[QuarterlyData$date == "1990-01-01"], col = "azure4") 

###################################################################
###### Plot a graph of correlations from 1948 to each quarter  ####
###################################################################

#Initialize vector to hold total correlations
QuarterlyData$TotalCor <- rep(NA, times = nrow(QuarterlyData))

#calculate correlations using data from beginning up to each point in the data
for (i in 2:nrow(QuarterlyData)){
  QuarterlyData$TotalCor[i] <- cor(QuarterlyData$UNRATE[1:i], QuarterlyData$Percentage[1:i]) 
}
#plot correlations using each new datapoint in our calculation
plot(QuarterlyData$date, QuarterlyData$TotalCor, type = 'l', 
     xlim = c(QuarterlyData$date[1],QuarterlyData$date[282]),
     ylim = c(-1,1),
     xlab = "End Date of Sample", ylab = "Correlation",
     main = " Correlation Between Unemployment Rate and Government Expenditures Starting in 1948",
     col = "blue")

#########################################
### plot correlations for each decade ###
#########################################

#pull out each decade
DecadeCorrelations <- (QuarterlyData[c(48, 88, 128, 168, 208, 248, 282), ])
#lot each and the correlation for that decade
plot(DecadeCorrelations$date, DecadeCorrelations$tenYearCor, type = 'l',
     xlim = c(QuarterlyData$date[40],QuarterlyData$date[282]),
     ylim = c(-1,1),
     xlab = "End Date of Sample", ylab = "Correlation",
     main = " Correlation Between Unemployment Rate and Government Expenditures by Decade",
     col = "red")
points(DecadeCorrelations$date, DecadeCorrelations$tenYearCor, pch = 19, col = "red")
abline(v = QuarterlyData$date[QuarterlyData$date == "2000-01-01"], col = "azure4") 

##########################
### Q5: Cherry Picking ###
##########################

#make uncorrelated variables and look at overall correlation
set.seed(123)
x <- rnorm(n = 256, mean = mean(QuarterlyData$UNRATE), sd = sd(QuarterlyData$UNRATE))
y <- rnorm(n = 256, mean = mean(QuarterlyData$Percentage), sd = sd(QuarterlyData$Percentage))
OverallCor <- cor(x,y)
cor.test(x,y) #check that correlation of the two variables is not statistically different than 0

#Calculate correlation for the last 256, 255, ... , 60 quarters 
#and find the max correlation over this time
xyFrame <- data.frame(x,y)
xyFrame$TotalCor <- rep(NA, times = 256)

for (i in 1:196){
  xyFrame$TotalCor[i] <- cor(xyFrame$x[i:256], xyFrame$y[i:256]) 
}
TopCor <- max(xyFrame$TotalCor[1:196])

#Now we will do this 1000 times
#initialize empty vectors to find these correlations for 1000 simulations
xyFrame$TotalCor <- rep(NA, times = 256)
TopCor <- rep(NA, times = 1000)
OverallCor <- rep(NA, times = 1000)
Results <- data.frame(TopCor, OverallCor)

#Calculate the correlations specified above for 1000 simulations, storing the "cherry picked"
#correlation and Overall correlation for each simulation
set.seed(123)
for (i in 1:1000){
  x <- rnorm(256, mean = mean(QuarterlyData$UNRATE), sd = sd(QuarterlyData$UNRATE))
  y <- rnorm(256, mean(QuarterlyData$Percentage), sd = sd(QuarterlyData$Percentage))
  xyFrame <- data.frame(x,y)
  for (a in 1:196){
    xyFrame$TotalCor[a] <- cor(xyFrame$x[a:256], xyFrame$y[a:256]) 
  }
  Results$TopCor[i] <- max(xyFrame$TotalCor[1:196])
  Results$OverallCor[i] <- cor(x,y)
}

#plot the top correlations in each sample
plot(Results$TopCor, ylim = c(-1,1),
     ylab = "Correlation",
    type = "p", col = "blue", pch = 19)

#add point for overall correlation in each sample
points(Results$OverallCor, type = 'p', col = 'red', bg = 'red', pch = 22)
legend(1, 1, legend=c("Top Correlation", "Overall Correlation"),
       col=c("blue", "red"), pch = c(19, 22), pt.bg = c("blue", "red"), cex = .55)

#make histograms showing the results of the top correlation and the overall correlations
hist(Results$TopCor, ylim = c(0, 300), xlab = "Correlation", main = "Overall correlation Vs Top Correlation")
hist(Results$OverallCor, add=TRUE, col=rgb(0,0,1,alpha=0.3))

#run a t-test of our top correlations vs our overall correlations and our top correlations vs Taylor's
#high correlation from Q1.
TaylorCor <- cor(Q1DF$UNRATE, Q1DF$Percentage)
t.test(Results$TopCor, Results$OverallCor) #There is a difference here, TopCor is significantly higher
t.test(Results$TopCor, mu = TaylorCor)#Top Correlation is nowhere near Taylor's correlation on average

#find max top correlation
max(Results$TopCor)

