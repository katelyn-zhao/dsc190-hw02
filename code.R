# Read txt file to data frame
video_data <- read.table("videodata.txt", header = TRUE, sep="")
video_multiple <- read.table("videoMultiple.txt", header = TRUE, sep="")

#Question 1
#Point Estimate Calculation
point_estimate_fraction <- nrow(video_data[video_data$time > 0,]) / nrow(video_data) #0.3736264

#Interval Estimate Calculation
n <- nrow(video_data)
estimate_error <- sqrt(point_estimate_fraction * (1- point_estimate_fraction)) / sqrt(n)
lower_interval_estimate_fraction <- point_estimate_fraction - (2 * estimate_error) #0.2722014
upper_interval_estimate_fraction <- point_estimate_fraction + (2 * estimate_error) #0.4750513

#Question 2
daily_gamers <- video_data[video_data$freq == 1,]
weekly_gamers <- video_data[video_data$freq == 2,]
monthly_gamers <- video_data[video_data$freq == 3,]
semesterly_gamers <- video_data[video_data$freq == 4,]

daily_point_estimate_fraction <- nrow(daily_gamers[daily_gamers$time > 0,]) / nrow(daily_gamers) #0.7777778
weekly_point_estimate_fraction <- nrow(weekly_gamers[weekly_gamers$time > 0,]) / nrow(weekly_gamers) #0.8571429
monthly_point_estimate_fraction <- nrow(monthly_gamers[monthly_gamers$time > 0,]) / nrow(monthly_gamers) #0.1111111
semesterly_point_estimate_fraction <- nrow(semesterly_gamers[semesterly_gamers$time > 0,]) / nrow(semesterly_gamers) #0.04347826

hist(daily_gamers$time, 
     main = "Daily Gamers", 
     xlab = "Hours Played",
     breaks=20)
hist(weekly_gamers$time, 
     main = "Weekly Gamers", 
     xlab = "Hours Played",
     breaks=20)
hist(monthly_gamers$time, 
     main = "Monthly Gamers", 
     xlab = "Hours Played",
     breaks=20)
hist(semesterly_gamers$time, 
     main = "Semesterly Gamers", 
     xlab = "Hours Played",
     breaks=20)

#Ouestion 3

#Look at the shape of the data
hist(video_data$time,
     main = 'Time Spent Playing Games a Week Prior to The Survey',
     xlab = "Hours Played",
     breaks=20)

#Use bootstrapping to calculate the confidence interval since the data is heavily skewed.
point_estimate_average <- mean(video_data$time) #1.242857
n_size<- dim(video_data)[1]
n_iterations <- 10000

bootstrap_means <- numeric(n_iterations)

set.seed(123)
for (i in 1:n_iterations) {
  bootstrap_sample <- sample(video_data$time, size = n_size, replace = TRUE)
  bootstrap_means[i] <- mean(bootstrap_sample)
}

lower_interval_estimate_average <- quantile(bootstrap_means, 0.025) #0.5988462  
upper_interval_estimate_average <- quantile(bootstrap_means, 0.975) #2.11217

hist(bootstrap_means, breaks = 30, col = "lightblue", main = "Bootstrap Distribution of the Mean", 
     xlab = "Mean", ylab = "Frequency")
abline(v = point_estimate_average, col = 'red', lty = 2, lwd = 2)
abline(v = lower_interval_estimate_average, col = "blue", lty = 2, lwd = 2)
abline(v = upper_interval_estimate_average, col = "blue", lty = 2, lwd = 2)

#Question 4

# Distribution of preferences for video games
table(video_data$like)

enjoys_videogames <- video_data[video_data$like == 2 | video_data$like == 3,]
dislikes_videogames <- video_data[video_data$like == 4 | video_data$like == 5,]
never_played <- video_data[video_data$like == 1,]

# Proportion of students who enjoy playing
prop_enjoy <- nrow(enjoys_videogames) / nrow(video_data)
prop_dislike <- nrow(dislikes_videogames) / nrow(video_data)
prop_never_played <- nrow(never_played) / nrow(video_data)

# Remove non-responses (99)
filtered_video_data <- video_data[video_data$like != 99, ]

# Plot histogram of video game preferences
hist(filtered_video_data$like, 
     breaks = 5,  # Adjust number of bins based on categories
     col = "blue", 
     main = "Distribution of Video Game Preferences", 
     xlab = "Preference (1 = Never Played, 2-3 = Enjoys, 4-5 = Dislikes)",
     ylab = "Frequency",
     xaxt = "n")  # Remove the default x-axis labels
axis(1, at = 1:5, labels = c("Never Played", "Enjoy (2)", "Enjoy (3)", "Dislike (4)", "Dislike (5)"))

# Reasons for liking video games 
reasons_liking <- colSums(video_multiple[, c("graphic", "relax", "coord", 
                                             "challenge", "master", "bored")], na.rm = TRUE)
# Reasons for disliking video games 
reasons_disliking <- colSums(video_multiple[, c("time", "frust", "lonely", "rules", "cost", "boring", "friends", "point")], na.rm = TRUE)


# Save the most common reasons for liking/disliking video games
reasons_for_likeliness <- list(
  "Reasons for Liking" = names(sort(reasons_liking, decreasing = TRUE)[1:4]),
  "Reasons for Disliking" = names(sort(reasons_disliking, decreasing = TRUE)[1:4])
)

# Barplot for reasons for liking video games
barplot(reasons_liking, 
        main = "Reasons for Liking Video Games", 
        col = "green", 
        xlab = "Reason", 
        ylab = "Count", 
        las = 2) 

# Barplot for reasons for disliking video games
barplot(reasons_disliking, 
        main = "Reasons for Disliking Video Games", 
        col = "red", 
        xlab = "Reason", 
        ylab = "Count", 
        las = 2)

#Question 5

#Question 6

#Advanced Analysis

#Hates math vs. Doesn't Hate Math Permutation Test
hates_math <- video_data[video_data$math == 1,] #29
likes_math <- video_data[video_data$math == 0,] #61

hist(hates_math$time,
     main='Hates Math',
     xlab = 'Hours Played',
     breaks=10)
hist(likes_math$time,
     main='Likes Math',
     xlab='Hours Played',
     breaks=10)

hates_math_mean = mean(hates_math$time) #1.172414
likes_math_mean = mean(likes_math$time) #1.296721

math_observed_difference <- hates_math_mean - likes_math_mean

n_permutations <- 10000  # Number of permutations
math_perm_diffs <- numeric(n_permutations)

for (i in 1:n_permutations) {
  shuffled_labels <- sample(video_data$math)  # Shuffle Hate_Math labels
  perm_diff <- mean(video_data$time[shuffled_labels == 1]) - mean(video_data$time[shuffled_labels == 0])
  math_perm_diffs[i] <- perm_diff
}

math_p_value <- mean(math_perm_diffs >= math_observed_difference) #0.4905

hist(math_perm_diffs, breaks = 30, main = "Permutation Distribution of Mean Differences", 
     xlab = "Difference in Means", col = "lightblue")
abline(v = math_observed_difference, col = "red", lwd = 2, lty = 2)

#Play If Busy Permutation Test
play_if_busy <- video_data[video_data$busy == 1,] #17
no_play_if_busy <- video_data[video_data$busy == 0,] #63

hist(play_if_busy$time,
     main='Plays if Busy',
     xlab = 'Hours Played',
     breaks=10)
hist(no_play_if_busy$time,
     main="Doesn't Play if Busy",
     xlab='Hours Played',
     breaks=10)

play_if_busy_mean <- mean(play_if_busy$time) #4.705882
no_play_if_busy_mean <- mean(no_play_if_busy$time) #0.5095238

busy_observed_difference <- play_if_busy_mean - no_play_if_busy_mean

n_permutations <- 10000  # Number of permutations
busy_perm_diffs <- numeric(n_permutations)

for (i in 1:n_permutations) {
  shuffled_labels <- sample(video_data$busy)  # Shuffle Play_if_busy labels
  perm_diff <- mean(video_data$time[shuffled_labels == 1]) - mean(video_data$time[shuffled_labels == 0])
  busy_perm_diffs[i] <- perm_diff
}

busy_p_value <- mean(busy_perm_diffs >= busy_observed_difference) #0

hist(busy_perm_diffs, breaks = 30, main = "Permutation Distribution of Mean Differences", 
     xlab = "Difference in Means", col = "lightblue")
abline(v = busy_observed_difference, col = "red", lwd = 2, lty = 2)

print(play_if_busy_mean)
print(no_play_if_busy_mean)