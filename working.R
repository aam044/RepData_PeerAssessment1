setwd("/Users/aam044/Documents/R_Programming/reproducible_research/Project1/RepData_PeerAssessment1")
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)

library(dplyr)

data <- read.csv(file="activity.csv", header=TRUE, sep=",", na.strings=c("NA","") )
str(data)



#What is mean total number of steps taken per day?

steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")


step_mean <- mean(steps_by_day$steps)
step_median <- median(steps_by_day$steps)

#Average daily activity pattern.

library(dplyr)
data.interval <- filter(data, 
                     !is.na(steps)) %>% 
  group_by(interval) %>%
  summarise(msteps=mean(steps)) 

plot(data.interval, type='l')

#which interval
data.interval[which.max(data.interval$msteps),]


#Imputing missing values

#Note that there are a number of days/intervals where there are missing values 
#(coded as NA). The presence of missing days may introduce bias into some calculations 
#or summaries of the data.

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
incomplete <- sum(!complete.cases(data))
incomplete_steps <- sum(is.na(data$steps))
print (incomplete_steps)

incomplete
data.imputed_na<-mutate(data, imputed = is.na(steps)) %>%
    mutate(steps = ifelse(is.na(steps), data.interval$msteps[match(interval, data.interval$interval)], steps))

steps_by_day.imputed <- aggregate(steps ~ date, data.imputed_na, sum)
hist(steps_by_day.imputed$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
step_mean <- mean(steps_by_day.imputed$steps)
step_median <- median(steps_by_day.imputed$steps)

print(paste("Median:", step_median, " Mean:", step_mean))

hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)

str(data)
data<-mutate(data, date=as.Date(factor(date))) %>%
    mutate(weekend = (wday(date) == 7 | wday(date) ==1))

wday(data$date)

steps_by_interval_i <- aggregate(steps ~ interval + weekend, data, mean)
View(steps_by_interval_i)
library(lattice)
View(data)

z <-c( "Weekend" , "Weekday" ) # Same number of values as there are panels
xyplot(a~x | y,data=test,strip=strip.custom(factor.levels=z))
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$weekend, 
       main="Average Steps by Interval",xlab="Interval", ylab="Steps",
       layout=c(1,2), type="l",
       strip=strip.custom(factor.levels=z))

#data.interval$steps[match(interval, data.interval$interval)]
#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. 
#For example, you could use the mean/median for that day, 
#or the mean for that 5-minute interval, etc.

#Create a new dataset that is equal to the original dataset but with the missing data filled in.

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?




