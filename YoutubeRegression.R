#####################################
###        Project 3              ###
###        Neven Grgic            ###
#####################################

#Regression analysis of youtube videos with and without advertisements. Videos switched from no pre-roll ad
#to having a pre-roll ad on Feb 5 or vice versa, however there was a mistake in that the wrong data was provided for 
#the project which threw off many of the project conclusions. Overall, the project was still a strong exercise of
#regression applying to a business problem for a youtuber wondering when ads are most effective for revenue and attention

#Create a function to read in the csv, add the video title as a column, and bind the video data together
folderdata = function(paths){
  dat = NULL
  for (i in 1:length(paths)){
    this = read.csv(paths[i], header = TRUE)
    this = cbind(this, paths[i])
    dat = rbind(dat, this)
  }
  return(dat)
}

#------------#
#   Part 1   #
#------------#

#############################
### Combine Revs and Vids ###
#############################

#use the function to read in revs and rename the columns
setwd("~/MS Courses/Quantitative Methods/Datasets/YouTube2013Attention/Revenues")
base = ("~/MS Courses/Quantitative Methods/Datasets/YouTube2013Attention/Revenues")
yt_paths = list.files(base, full.names = FALSE)
revs = folderdata(yt_paths)
names(revs) = c('Date', 'Total.earn', 'AFV.earn', 'YT.earn', 'Transactions', 'videoid')

#use the function to read in vids and rename columns
setwd("~/MS Courses/Quantitative Methods/Datasets/YouTube2013Attention/Videos")
base = ("~/MS Courses/Quantitative Methods/Datasets/YouTube2013Attention/Videos")
yt_paths = list.files(base, full.names = FALSE)
vids = folderdata(yt_paths)
names(vids) = c('Date', 'views', 'est_mins_watched', 'average_watch_time', 'unique_cookies', 'videoid')

#Now that we have revs and vids loaded:
#get the unique id of each video out of the file name so that we can merge revs and vids
revs$videoid2 <- substring(revs$videoid, 8) #remove 'Revenue' from each file name
revs$videoid2 <- sub("\\..*", '', revs$videoid2) #remove '.csv' from each file name in revs

vids$videoid2 <- substring(vids$videoid, 6) #remove 'video' from each file name
vids$videoid2 <- sub("\\..*", '', vids$videoid2) #remove '.csv' from each file name

#format dates in each dataset. Dates will need to match in order to merge revs and vids properly
#first in vids, where we have different date formatting in different parts of the data
a <- as.Date(vids$Date, format = '%b %d, %Y')
b <- as.Date(vids$Date, format = '%d-%b-%y')
a[is.na(a)] <- b[!is.na(b)]
vids$Date <- a

#convert Revs$Date to date, only has one date format so this is simpler
revs$Date <- as.Date(revs$Date, format = '%b %d, %Y')

#format Total.earn in revs as a numeric decimal. factor was causing rounding issue and not matching excel files
x <- as.character(revs$Total.earn)
y <- as.numeric(sub(pattern = '\\$', replacement = '', x))

revs$Total.earn <- y

#merge revs and vids into one dataframe, joining the unique date/video pairs
DF <- merge(revs, vids, by = c('Date', 'videoid2'))

#pull out the columns of interest stated in the prompt and rename them
DF <- DF[ , c('Date', 'videoid2', 'Total.earn', 'views', 'average_watch_time')]
names(DF) <- c('Date', 'video_id', 'daily_earnings', 'views', 'average_watch_time')

#create variable for earnings_per_view
DF$earnings_per_view <- DF$daily_earnings/DF$views


#############################
### add on adinstream.csv ###
#############################

#read in instream dataset and rename columns
setwd('~/MS Courses/Quantitative Methods/Datasets')
instream <- read.csv('adinstream.csv', header = TRUE)
names(instream) <- c('video_id', 'id', 'annoy_before', 'annoy_after')

#make names of videoid match the rev/vids datasets. Certain charcters/names don't currently match
instream$video_id <- sub('Regression', 'Reg', instream$video_id)
instream$video_id <- sub('whatisintro', 'WhatIs', instream$video_id)
instream$video_id <- toupper(instream$video_id)
DF$video_id <- toupper(DF$video_id)

#merge df and instream and pull out useful columns to come to our final dataset
df <- merge(DF, instream, by = 'video_id')

##########################
### Summarize the data ###
##########################

#summarize data and calculate standard errors
summary(df)
sd(df$daily_earnings)
sd(df$views)
sd(df$average_watch_time)

# produce the box plots of a few variables
boxplot(df$daily_earnings, main = 'Daily Earnings')
boxplot(df$views, main = 'Views')
boxplot(df$average_watch_time, main = 'Average Watch Time')

#--------#
# Part 2 #
#--------#

##########
### Q1 ###
##########

#pull out pre-period
preperiod <- df[df$Date < '2013-02-05', ]

#run a regression of average watch time on annoy before
preperiodRegression1 <- lm(preperiod$average_watch_time ~ preperiod$annoy_before)
summary(preperiodRegression1)

#run a regression of views on annoy before
preperiodRegression2 <- lm(preperiod$views ~ preperiod$annoy_before)
summary(preperiodRegression2)

#check results of regression with a t-test to see if we get the same conclusion
#get all data prior to feb 5 and split into a dataframe for annoy before = 1 and annoy before = 0
#separate this way since one video has both annoy before and annoy after. 
prePeriodBefore <- df[df$Date < '2013-02-05' & df$annoy_before == 1, ]
prePeriodAfter <- df[df$Date < '2013-02-05' & df$annoy_before == 0, ]

#conduct a test to see if there is a difference between the average time watched and average 
#views for videos with an ad at the beginning vs an ad at the end
t.test(prePeriodBefore$average_watch_time, prePeriodAfter$average_watch_time)
t.test(prePeriodBefore$views, prePeriodAfter$views) #both tests show statistical significance

##########
### Q2 ###
##########

#do a correlation test, plot, and regression for daily earnings using average watch time
cor.test(df$average_watch_time, df$daily_earnings) #Very weak, but statistically significant positive correlation
plot(df$average_watch_time, df$daily_earnings, ylab = 'Daily Earnings', xlab = 'Average Watch Time', main = 'Daily Earnings vs. Average Watch Time', pch = 19, col = 'blue')
Q2Regression1 <- lm(df$daily_earnings ~ df$average_watch_time)
abline(Q2Regression1, col = 'red')
summary(Q2Regression1)

#do a correlation test, plot, and regression for daily earnings using average views
cor.test(df$views, df$daily_earnings) #pretty strong positive correlation, statistically significant positive correlation
plot(df$views, df$daily_earnings, ylab = 'Daily Earnings', xlab = 'Views', main = 'Daily Earnings Vs. Views', pch = 19, col = 'blue')
Q2Regression2 <- lm(df$daily_earnings ~ df$views)
abline(Q2Regression2, col = 'red')
summary(Q2Regression2)

#do a correlation test, plot, and regression for views using average watch time
cor.test(df$average_watch_time, df$views)#very weak, statistically significant positive relationship
plot(df$average_watch_time, df$views, xlab = 'Average Watch Time', ylab = 'Views', main = 'Views Vs Average Watch Time', pch = 19, col = 'blue') 
Q2Regression3 <- lm(df$views ~ df$average_watch_time)
abline(Q2Regression3, col = 'red')
summary(Q2Regression3)

##########
### Q3 ###
##########

#initialize an empty vector to hold 'post'
df$post <- rep(NA, times = 14669)

#loop through the dates, marking post a 1 if it is on or after feb 5 2013 and 0 if before
for (i in 1:length(df$post)){
  if (df$Date[i] >= '2013-02-05') {
    df$post[i] <- 1
   } else {
    df$post[i] <- 0
  }
}

postRegression <- lm(df$daily_earnings~df$post)
summary(postRegression) #estimate of B1 is statistically significant
#interpretation -> if the video was in the post set, on average it had .022299 less earnings per day

##########
### Q4 ###
##########

#initialize empty vector for annoy_later
df$annoy_later <- rep(NA, times = 14669)

#loop through df marking a 1 if the video switched to a pre-roll ad on Feb 5
for (i in 1:length(df$annoy_later)){
  if (df$annoy_after[i] == 1 & df$annoy_before[i] == 0){
    df$annoy_later[i] <- 1
  } else {
    df$annoy_later[i] <- 0
  }
}

annoy_later_Regression <- lm(df$daily_earnings~df$annoy_later)
summary(annoy_later_Regression)
#The estimate of B1 is statistically significant
#interpretation -> if the video switched from no ad to a pre roll add on Feb 5,
#on average that video earned .051401 more in average earnings per day than the intercept

##########
### Q5 ###
##########

combinedRegression = lm(daily_earnings ~ annoy_later + post + post:annoy_later, data = df)
summary(combinedRegression)
#B3 is statistically significant.
#interpretation -> if a video was both prerolling ads from Feb 5 onward AND the date was 
#Feb 5 or later, the video earned .015852 more in average earnings per day than the intercept

##########
### Q6 ###
##########


#repeat #3 with average_watch_time as the dependent variable
postRegressionWatch <- lm(df$average_watch_time~df$post)
summary(postRegressionWatch) #estimate of B1 is statistically significant
#interpretation -> if the video was in the post set, on average it had 0.10869 more average_time_watched
#than the intercept

#repeat #4 with average_watch_time as the dependent variable
annoy_later_RegressionWatch <- lm(df$average_watch_time~df$annoy_later)
summary(annoy_later_RegressionWatch)
#The estimate of B1 is statistically significant
#interpretation -> if the video switched from no ad to a pre roll add on Feb 5,
#on average that video was watched .24082 minutes longer than the intercept

#repeat #5 with average_watch_time as the dependent variable
combinedRegression = lm(average_watch_time ~ annoy_later + post + post:annoy_later, data = df)
summary(combinedRegression)
#B3 is statistically significant.
#interpretation -> if a video was both prerolling ads from Feb 5 onward AND the date of the 
#video was Feb 5 or later, the video was watched .31888 minutes longer than the intercept on average
