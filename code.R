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

# Assignment to two groups
video_data$likes_games <- ifelse(video_data$like == 2, 3 | video_data$like == 4, 5)
likes_videogames <- video_data[video_data$like == 2 | video_data$like == 3,]
dislikes_videogames <- video_data[video_data$like == 4 | video_data$like == 5,]

# Gender comparison
gender_table <- table(video_data$likes_games, video_data$sex)
colnames(gender_table) <- c("Female", "Male")

# Work comparison
work_table <- table(video_data$likes_games, video_data$work > 0)
colnames(work_table) <- c("Doesn't Work", "Works")

# Computer ownership comparison
pc_table <- table(video_data$likes_games, video_data$own)
colnames(pc_table) <- c("No PC", "Owns PC")

# Gender comparison visualization
barplot(gender_table, beside = TRUE, col = c("red", "blue"),
        main = "Video Game Preferences by Gender",
        xlab = "Gender", ylab = "Number of Students",
        legend = c("Doesn't Like", "Likes to Play"))

# Work comparison visualization
barplot(work_table, beside = TRUE, col = c("red", "blue"),
        main = "Video Game Preferences by Work Status",
        xlab = "Work Status", ylab = "Number of Students",
        legend = c("Doesn't Like", "Likes to Play"))

# Computer ownership comparison visualization
barplot(pc_table, beside = TRUE, col = c("red", "blue"),
        main = "Video Game Preferences by Computer Ownership",
        xlab = "Computer Ownership", ylab = "Number of Students",
        legend = c("Doesn't Like", "Likes to Play"))

# Question 6 --------------------------------------------------------------

# Target distribution for comparison
target_distribution <- c("A" = 0.2, "B" = 0.3, "C" = 0.4, "D or F" = 0.1)

# Combine D and F into "D or F" to match the target distribution
video_data$grade_collapsed <- ifelse(video_data$grade == 1 | video_data$grade == 0, "D or F", 
                                     ifelse(video_data$grade == 2, "C", 
                                            ifelse(video_data$grade == 3, "B", "A")))

# Recalculate observed proportions with D and F combined
observed_grades_collapsed <- table(video_data$grade_collapsed)
observed_proportion_collapsed <- prop.table(observed_grades_collapsed)

# Target grade distribution
target_distribution <- data.frame(
  Grade = c("A", "B", "C", "D/F"),
  Target_Proportion = c(0.20, 0.30, 0.40, 0.10)
)

# Calculate observed grade distribution from the data
grade_counts <- table(video_data$grade)
grade_proportions <- prop.table(grade_counts)

# Convert to a data frame for plotting
observed_distribution <- data.frame(
  Grade = c("A", "B", "C", "D/F"),
  Observed_Proportion = c(grade_proportions["4"], grade_proportions["3"], 
                          grade_proportions["2"], sum(grade_proportions["1"], grade_proportions["0"]))
)

# Merge target and observed distributions
grade_comparison <- merge(target_distribution, observed_distribution, by = "Grade")



#Comparing results including non respondents
grade_counts_with_nonrespondents <- c(4, 8, 52, 31) 
names(grade_counts_with_nonrespondents) <- c(1, 2, 3, 4)
grade_table_with_nonrespondents <- as.table(grade_counts_with_nonrespondents)
grade_proportions_with_nonrespondents <- prop.table(grade_counts_with_nonrespondents)

observed_distribution_with_nonrespondents <- data.frame(
  Grade = c("A", "B", "C", "D/F"),
  Observed_Proportion_With_Nonrespondents = c(grade_proportions_with_nonrespondents["4"], grade_proportions_with_nonrespondents["3"], 
                          grade_proportions_with_nonrespondents["2"], grade_proportions_with_nonrespondents["1"])
)
  
  grade_comparison_with_nonrespondents <- merge(target_distribution, observed_distribution_with_nonrespondents, by = "Grade")
  
  # Bar Graphs
  grades <- c("A", "B", "C", "D/F")
  target_proportion <- c(0.2, 0.3, 0.4, 0.1)
  observed_proportion <- c(0.34065934, 0.57142857, 0.08791209, 0.0)
  observed_proportion_with_nonrespondents <- c(0.32631579, 0.54736842, 0.08421053, 0.04210526)
  
  proportions_with_nonrespondents <- rbind(target_proportion, observed_proportion_with_nonrespondents)
  proportions <- rbind(target_proportion, observed_proportion)
  colors <- c(rgb(1,0,0,0.25), rgb(0,0,1,0.25))
  
  bar_positions <- barplot(proportions,
          beside = TRUE,
          names.arg = grades,
          col = colors,
          ylim = c(0, 1),
          main = "Comparison of Target and Observed Grade Distributions",
          ylab = "Proportion", xlab = "Grade",
          legend.text = c("Target Proportion", "Observed Proportion"))
  
  y_ticks <- seq(0, 1, by = 0.1)
  for (y in y_ticks) {
    segments(x0 = min(bar_positions) - 0.9, x1 = max(bar_positions) + 0.5, 
             y0 = y, y1 = y, col = "gray", lty = "dotted")
  }
  
  bar_positions <- barplot(proportions_with_nonrespondents,
          beside = TRUE,
          names.arg = grades,
          col = colors,
          ylim = c(0, 1),
          main = "Grade Distributions With Nonrespondents",
          ylab = "Proportion", xlab = "Grade",
          legend.text = c("Target Proportion", "Observed Proportion"))
  
  y_ticks <- seq(0, 1, by = 0.1)
  for (y in y_ticks) {
    segments(x0 = min(bar_positions) - 0.9, x1 = max(bar_positions) + 0.5, 
             y0 = y, y1 = y, col = "gray", lty = "dotted")
  }
  

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
