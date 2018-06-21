# reading the dataframe
df <- read.csv("activity.csv")

# Total steps per day
daily_steps <- with(df, tapply(steps, date, sum, na.rm=T))
# mean & median
mean_steps <- mean(daily_steps)
median_steps <- median(daily_steps)
# histogram
library(ggplot2)
qplot(daily_steps, geom="histogram", 
      bins=10, fill=I("red"),
      main="Histogram of the total number of steps\n taken each day",
      ylim=c(0, 25),
      xlab="number of steps per day",
      ylab="Frequency")

# Average number of steps in each day interval
interval_steps_mean <- with(df, tapply(steps, interval, mean, na.rm=T))
# plot
average_activity <- aggregate(steps ~ interval, data=df, mean)
ggplot(average_activity, aes(interval, steps)) + 
        geom_line() + 
        ggtitle("Average daily activity pattern")+
        xlab("Interval")+
        ylab("Steps")


# interval with max average steps
max_steps_interval <- as.integer(names(which.max(interval_steps_mean)))
# correspondin interval in hh:mm
hh <- max_steps_interval %/% 60
mm <- max_steps_interval %% 60




# number of NA
na_number <- sum(is.na(df))



# defining the filling function
my_function_fill_na <- function(x_steps, y_interval){
        if(is.na(x_steps)){
                return(as.integer(interval_steps_mean[as.character(y_interval)]))
        } else {
                return(x_steps)
        }
}
# applying the filling function across the rows of the new dataframe
df_filled <- data.frame(df)
df_filled$steps <- mapply(my_function_fill_na, df_filled$steps, df_filled$interval)




# Total steps per day
daily_steps_filled <- with(df_filled, tapply(steps, date, sum, na.rm=T))
# mean & median
mean_steps_filled <- mean(daily_steps_filled)
median_steps_filled <- median(daily_steps_filled)
# histogram
library(ggplot2)
qplot(daily_steps_filled, geom="histogram", 
      bins=10, fill=I("red"),
      main="Histogram of the total number of steps\n taken each day from filled dataframe",
      ylim=c(0, 25),
      xlab="number of steps per day",
      ylab="Frequency")




# Date and Time conversion
Sys.setlocale("LC_TIME", "English")
# defining the weekend/weekday filling function
my_function_weekend <- function(x_date){
        if(weekdays(as.Date(x_date)) %in% c("Saturday", "Sunday")){
                return("weekend")
        } else {
                return("weekday")
        }
}
# applying the filling function across the rows of the new dataframe
df_filled$days <- mapply(my_function_weekend, df_filled$date)





average_activity_filled <- aggregate(steps ~ interval + days, data=df_filled, mean)
ggplot(average_activity_filled, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(days ~ .)+
        ggtitle("Average daily activity pattern")+
        xlab("Interval")+
        ylab("Steps")






