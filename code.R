# Read txt file to data frame
video_data <- read.table("videodata.txt", header = TRUE, sep="")
video_multiple <- read.table("videoMultiple.txt", header = TRUE, sep="")

#Question 1
#Point Estimate Calculation
point_estimate_fraction <- dim(video_data[video_data$time > 0,])[1] / dim(video_data)[1] #0.3736264

#Interval Estimate Calculation
n <- dim(video_data)[1]
estimate_error <- sqrt(point_estimate_fraction * (1- point_estimate_fraction)) / sqrt(n)
lower_interval_estimate_fraction <- point_estimate_fraction - (2 * estimate_error) #0.2722014
upper_interval_estimate_fraction <- point_estimate_fraction + (2 * estimate_error) #0.4750513

#Question 2
daily_gamers <- video_data[video_data$freq == 1,]
weekly_gamers <- video_data[video_data$freq == 2,]
monthly_gamers <- video_data[video_data$freq == 3,]
semesterly_gamers <- video_data[video_data$freq == 4,]

daily_point_estimate_fraction <- dim(daily_gamers[daily_gamers$time > 0,])[1] / dim(daily_gamers)[1] #0.7777778
weekly_point_estimate_fraction <- dim(weekly_gamers[weekly_gamers$time > 0,])[1] / dim(weekly_gamers)[1] #0.8571429
monthly_point_estimate_fraction <- dim(monthly_gamers[monthly_gamers$time > 0,])[1] / dim(monthly_gamers)[1] #0.1111111
semesterly_point_estimate_fraction <- dim(semesterly_gamers[semesterly_gamers$time > 0,])[1] / dim(semesterly_gamers)[1] #0.04347826

hist(daily_gamers$time, 
     main = "Daily Gamers", 
     xlab = "Hours Played",
     breaks=19)
hist(weekly_gamers$time, 
     main = "Weekly Gamers", 
     xlab = "Hours Played",
     breaks=19)
hist(monthly_gamers$time, 
     main = "Monthly Gamers", 
     xlab = "Hours Played",
     breaks=19)
hist(semesterly_gamers$time, 
     main = "Semesterly Gamers", 
     xlab = "Hours Played",
     breaks=19)

#Ouestion 3
point_estimate_average <- mean(video_data$time) #1.242857

#Look at the shape of the data
hist(video_data$time,
     main = 'Time Spent Playing Games a Week Prior to The Survey',
     xlab = "Hours Played",
     breaks=24)

#Use bootstrapping to calculate the confidence interval since the data is heavily skewed.
n_size<- dim(video_data)[1]
n_iterations <- 4000

bootstrap_means <- numeric(n_iterations)

set.seed(123)
for (i in 1:n_iterations) {
  bootstrap_sample <- sample(video_data$time, size = n_size, replace = TRUE)
  bootstrap_means[i] <- mean(bootstrap_sample)
}

lower_interval_estimate_average <- quantile(bootstrap_means, 0.025) #0.5911538 
upper_interval_estimate_average <- quantile(bootstrap_means, 0.975) #2.101346

hist(bootstrap_means, breaks = 30, col = "lightblue", main = "Bootstrap Distribution of the Mean", 
     xlab = "Mean", ylab = "Frequency")
abline(v = point_estimate_average, col = 'red', lty = 2, lwd = 2)
abline(v = lower_interval_estimate_average, col = "blue", lty = 2, lwd = 2)
abline(v = upper_interval_estimate_average, col = "blue", lty = 2, lwd = 2)

#Question 4