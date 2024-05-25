install.packages("gridExtra")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("openxlsx")

library(gridExtra)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stats)
library(agricolae)

data <- read_excel("~/Desktop/basketball_players-486.xlsx")

# Step 1: Read the data from the Excel file
basketball_data <- data

# Step 2: Explore the data
head(basketball_data)
summary(basketball_data)

# Step 3:  Clean the data by removing rows with non-finite values for Cm or PTS

cleaned_data <- basketball_data %>%
  filter(is.finite(Cm) & is.finite(PTS))

# Step 4: Visualize the data for Heights (Cm)

# Pie Chart 
# Calculate frequency of each height category
height_freq <- table(cleaned_data$Cm)

# Sort the heights by frequency in descending order
sorted_heights <- sort(height_freq, decreasing = TRUE)

# Keep only the top N categories
top_categories <- names(sorted_heights)[1:10]  # Adjust the number as needed

# Filter the data to keep only the top categories
top_heights <- cleaned_data[cleaned_data$Cm %in% top_categories, ]

# Calculate frequency of top categories
top_height_freq <- table(top_heights$Cm)

# Plot the pie chart
pie(top_height_freq, 
    labels = paste(names(top_height_freq), "(", top_height_freq, ")", sep = ""), 
    main = "Top Player 10 Heights", 
    col = rainbow(length(top_height_freq)))



# Histogram
hist(cleaned_data$Cm, 
     main = "Histogram of Player Heights", 
     xlab = "Height (cm)", 
     col = "blue", 
     border = "black")

# Dot Chart (Nokta Grafiği)

subset_data <- basketball_data[1:486, ]

# Plot dot chart for the points of the first 20 players
dotchart(subset_data$Cm,
         main = "Dot Chart of Player Height (Cm)",
         xlab = "Points (Cm)")

# Subset the data to include only the first 20 players
subset_data <- cleaned_data[1:20, ]

# Set up the plot with larger margins and label size
par(mar = c(7, 4, 4, 2) + 0.1) # Adjust the margins as needed

# Plot dot chart for the points of the first 20 players with their names
dotchart(subset_data$Cm, 
         labels = subset_data$Player_Name,  
         main = "Dot Chart of Player Height (Top 20 Players)", 
         xlab = "Height (Cm)",
         cex = 0.8)  # Adjust the character expansion for label size

# Scatter Plot (Dağılım Grafiği)
plot(cleaned_data$Cm, 
     main = "Scatter Plot of Player Heights", 
     xlab = "Index", 
     ylab = "Height (cm)", 
     pch = 19, 
     col = "red")

# Pareto Chart or Bar Graph (Çubuk Grafik)
barplot(table(cleaned_data$Cm), 
        main = "Bar Graph of Player Heights", 
        xlab = "Height (cm)", 
        ylab = "Frequency", 
        col = "green")

# Stem-and-Leaf Plot (Kök ve Yaprak Grafiği)
stem(cleaned_data$Cm, scale = 2)

# Box Plot (Kutu Grafiği)
boxplot(cleaned_data$Cm,
        main = "Box Plot of Player Heights",
        ylab = "Height (cm)",
        col = "orange")

# Distribution Plot (Dağılım Grafiği)
ggplot(cleaned_data, aes(x = Cm)) + 
  geom_density(fill = "skyblue") + 
  labs(title = "Density Plot of Player Heights", x = "Height (cm)", y = "Density")

# Step 4.1: Visualize the data for PTS (Points)

# Pie Chart (Daire Grafiği)

# Calculate frequency of each points category
points_freq <- table(basketball_data$PTS)

# Sort the points by frequency in descending order
sorted_points <- sort(points_freq, decreasing = TRUE)

# Keep only the top N categories
top_categories <- names(sorted_points)[1:10]  

# Filter the data to keep only the top categories
top_points <- basketball_data[basketball_data$PTS %in% top_categories, ]

# Calculate frequency of top categories
top_points_freq <- table(top_points$PTS)

# Plot the pie chart
pie(top_points_freq, 
    labels = paste(names(top_points_freq), "(", top_points_freq, ")", sep = ""), 
    main = "Top Player 10 Points (PTS)", 
    col = rainbow(length(top_points_freq)))

# Histogram
hist(basketball_data$PTS,
     main = "Histogram of Player Points (PTS)",
     xlab = "Points (PTS)",
     col = "blue",
     border = "black")





# Dot Chart (Nokta Grafiği)

subset_data <- basketball_data[1:486, ]

# Plot dot chart for the points of the first 20 players
dotchart(subset_data$PTS,
         main = "Dot Chart of Player Points (PTS)",
         xlab = "Points (PTS)")

# Subset the data to include only the first 20 players
subset_data <- cleaned_data[1:20, ]

# Set up the plot with larger margins and label size
par(mar = c(7, 4, 4, 2) + 0.1) # Adjust the margins as needed

# Plot dot chart for the points of the first 20 players with their names
dotchart(subset_data$PTS, 
         labels = subset_data$Player_Name, 
         main = "Dot Chart of Player Points (Top 20 Players)", 
         xlab = "Points (PTS)",
         cex = 0.8)  # Adjust the character expansion for label size


# Scatter Plot (Dağılım Grafiği)
plot(basketball_data$PTS,
     main = "Scatter Plot of Player Points (PTS)",
     xlab = "Index",
     ylab = "Points (PTS)",
     pch = 19,
     col = "red")

# Pareto Chart or Bar Graph (Çubuk Grafik)
barplot(table(basketball_data$PTS),
        main = "Bar Graph of Player Points (PTS)",
        xlab = "Points (PTS)",
        ylab = "Frequency",
        col = "green")

# Stem-and-Leaf Plot (Kök ve Yaprak Grafiği)
stem(basketball_data$PTS, scale = 2)

# Box Plot (Kutu Grafiği)
boxplot(cleaned_data$PTS,
        main = "Box Plot of Player Points (PTS)",
        ylab = "Points (PTS)",
        col = "orange")

# Distribution Plot (Dağılım Grafiği)
ggplot(cleaned_data, aes(x = PTS)) +
  geom_density(fill = "skyblue") +
  labs(title = "Density Plot of Player Points (PTS)", x = "Points (PTS)", y = "Density")

# Step 6: Normality test
shapiro_test <- shapiro.test(basketball_data$PTS)
shapiro_test

# Step 7: Point estimates and confidence intervals
mean_pts <- mean(basketball_data$PTS)
sd_pts <- sd(basketball_data$PTS)
n <- length(basketball_data$PTS)
margin_of_error <- qt(0.975, n-1) * (sd_pts / sqrt(n))
confidence_interval <- c(mean_pts - margin_of_error, mean_pts + margin_of_error)
confidence_interval


# Step 8: Hypothesis Testing

# Hypothesis 1: Is the average height of basketball players different from 2 meters?
t.test(basketball_data$Cm, mu = 200)

# Hypothesis 2: Is the average PTS different from 3?
t.test(basketball_data$PTS, mu = 3)

# Hypothesis 3: Is there any relation between the height of the players and the points that they make?

# Perform linear regression
linear_model <- lm(PTS ~ Cm, data = basketball_data)

# Extract p-value for the slope coefficient (height)
p_value <- summary(linear_model)$coefficients[2, 4]

# Print the p-value
cat("The p-value for the slope coefficient is:", p_value, "\n")

if (p_value < 0.05) {
  cat("Reject the null hypothesis: There is a significant relationship between height and points scored.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no significant relationship between height and points scored.\n")
}

# Hypothesis 4: Is there any relation between age and points that they make?
linear_model_age_pts <- lm(PTS ~ Age, data = cleaned_data)
summary(linear_model_age_pts)

# Extract p-value for the slope coefficient (age)
p_value_age_pts <- summary(linear_model_age_pts)$coefficients[2, 4]

# Print the p-value
cat("The p-value for the slope coefficient (age) predicting points scored is:", p_value_age_pts, "\n")

# Hypothesis testing
if (p_value_age_pts < 0.05) {
  cat("Reject the null hypothesis: There is a significant relationship between age and points scored.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no significant relationship between age and points scored.\n")
}

# Additional: Visualize the regression line for age and points scored
ggplot(cleaned_data, aes(x = Age, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot with Regression Line (Age vs Points Scored)", x = "Age", y = "Points Scored")

# Hypothesis 5: Is there a relation between the position of the players and the points that they make?

# Group similar positions
cleaned_data$Pos_Group <- ifelse(cleaned_data$Pos %in% c("SF", "SG"), "Guard",
                                 ifelse(cleaned_data$Pos %in% c("PG", "C"), "Center", "Forward"))

# Hypothesis Testing for Position Group and Points Scored
anova_result <- aov(PTS ~ Pos_Group, data = cleaned_data)

# Extract p-value for position group
p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]

# Print the p-value
cat("The p-value for the ANOVA test of position group predicting points scored is:", p_value, "\n")

# Hypothesis testing for position group
if (p_value < 0.05) {
  cat("Reject the null hypothesis: There is a significant difference in points scored by players in different position groups.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no significant difference in points scored by players in different position groups.\n")
}

# Step 10: Shapiro-Wilk test
cat("Shapiro-Wilk Test Result:\n")
cat("Test Statistic:", shapiro_test$statistic, "\n")
cat("P-value:", shapiro_test$p.value, "\n")

# Step 11: ANOVA
anova_result <- aov(PTS ~ Cm, data = basketball_data)
summary(anova_result)

# Step 12: Linear regression model
linear_model <- lm(PTS ~ Cm, data = basketball_data)
summary(linear_model)

# Step 13: Wilcoxon test
wilcox.test(basketball_data$PTS, mu = 0, alternative = "two.sided", exact = FALSE)

# Step 14: Spearman correlation
correlation_result <- cor.test(basketball_data$Age, basketball_data$PTS, method = "spearman", exact = FALSE)
print(correlation_result)

# Step 14: ANOVA for Nationality and Points Scored
anova_nationality <- aov(PTS ~ Nat, data = cleaned_data)
summary(anova_nationality)

# Extract p-value for nationality
p_value_nationality <- summary(anova_nationality)[[1]][["Pr(>F)"]][1]

# Print the p-value
cat("The p-value for the ANOVA test of nationality predicting points scored is:", p_value_nationality, "\n")

# Hypothesis testing for nationality
if (p_value_nationality < 0.05) {
  cat("Reject the null hypothesis: There is a significant difference in points scored among players of different nationalities.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no significant difference in points scored among players of different nationalities.\n")
}

# Additional: Boxplot for nationality and points scored
ggplot(cleaned_data, aes(x = Nat, y = PTS)) +
  geom_boxplot() +
  labs(title = "Boxplot of Points Scored by Nationality", x = "Nationality", y = "Points Scored")

# Step 15: ANOVA for Team and Points Scored
anova_team <- aov(PTS ~ Team_Name, data = cleaned_data)
summary(anova_team)

# Extract p-value for team
p_value_team <- summary(anova_team)[[1]][["Pr(>F)"]][1]

# Print the p-value
cat("The p-value for the ANOVA test of team predicting points scored is:", p_value_team, "\n")

# Hypothesis testing for team
if (p_value_team < 0.05) {
  cat("Reject the null hypothesis: There is a significant difference in points scored among players from different teams.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no significant difference in points scored among players from different teams.\n")
}

# Additional: Boxplot for team and points scored
ggplot(cleaned_data, aes(x = Team_Name, y = PTS)) +
  geom_boxplot() +
  labs(title = "Boxplot of Points Scored by Team", x = "Team Name", y = "Points Scored")

# Step 16: Perform F-test for comparing variances of player heights (Cm) between two groups (positions)

# Split the data into two groups based on position (Pos)
group1 <- cleaned_data %>% filter(Pos %in% c("SF", "SG", "PG")) # Guards
group2 <- cleaned_data %>% filter(Pos %in% c("C", "PF", "F", "G")) # Others

# Perform F-test for comparing variances
f_test_result <- var.test(group1$Cm, group2$Cm)

# Print the result
print(f_test_result)

# Step 17: Perform Scheffe test for multiple comparisons

# Convert 'Pos' to factor
data$Pos <- as.factor(data$Pos)

# Clean the data by removing rows with non-finite values for Cm or PTS
cleaned_data <- data %>%
  filter(is.finite(Cm) & is.finite(PTS))

# Perform ANOVA for positions and points scored
anova_result <- aov(PTS ~ Pos, data = cleaned_data)

# Perform Scheffe test for multiple comparisons
scheffe_result <- scheffe.test(anova_result, "Pos")

# Print the Scheffe test results
print(scheffe_result)

# Step 18: Perform Two-way ANOVA for comparing player heights (Cm) between positions (Pos) and teams (Team_Name)
anova_result <- aov(Cm ~ Pos * Team_Name, data = cleaned_data)

# Print the ANOVA table
print(summary(anova_result))

# Step 19: Perform the sign test for comparing the median points (PTS) to a known value

# Known median value of points (PTS)
known_median <- 12.9

# Perform the sign test
sign_test_result <- binom.test(sum(cleaned_data$PTS > known_median), length(cleaned_data$PTS), p = 0.5, alternative = "two.sided")

# Print the sign test result
print(sign_test_result)

# Step 20: Goodness of Fit test

# Linear Regression Model
linear_model <- lm(PTS ~ Cm, data = cleaned_data)
model_summary <- summary(linear_model)

# Goodness of Fit for Linear Regression
cat("R-squared: ", model_summary$r.squared, "\n")
cat("Adjusted R-squared: ", model_summary$adj.r.squared, "\n")
cat("Residual Standard Error: ", model_summary$sigma, "\n")

# Visualize the model fit
ggplot(cleaned_data, aes(x = Cm, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot with Regression Line (Height vs Points Scored)", x = "Height (cm)", y = "Points Scored")

# Step 21: Chi-Square test for height of the players

# Bin the height data into categories
cleaned_data$Height_Bin <- cut(cleaned_data$Cm,
                               breaks = c(170, 180, 190, 200, 210, 220),
                               labels = c("170-180", "180-190", "190-200", "200-210", "210-220"))

# Calculate observed frequencies of height bins
observed_freq <- table(cleaned_data$Height_Bin)

# Define expected frequencies (hypothetical)
# For example, you might expect that 20% of players fall in each bin
total_players <- sum(observed_freq)
expected_freq <- rep(total_players / length(observed_freq), length(observed_freq))

# Perform Chi-squared goodness of fit test
chi_squared_test <- chisq.test(x = observed_freq, p = expected_freq / sum(expected_freq))

# Print Chi-squared test results
cat("Chi-squared test statistic: ", chi_squared_test$statistic, "\n")
cat("P-value: ", chi_squared_test$p.value, "\n")

if (chi_squared_test$p.value < 0.05) {
  cat("Reject the null hypothesis: The observed distribution does not fit the expected distribution.\n")
} else {
  cat("Fail to reject the null hypothesis: The observed distribution fits the expected distribution.\n")
}

# Additional: Visualize observed vs. expected frequencies
observed_df <- as.data.frame(observed_freq)
expected_df <- data.frame(Var1 = names(observed_freq), Freq = expected_freq)

# Combine data for plotting
combined_df <- rbind(data.frame(Type = "Observed", observed_df), data.frame(Type = "Expected", expected_df))

# Plot observed vs expected frequencies
ggplot(combined_df, aes(x = Var1, y = Freq, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Observed vs Expected Frequencies of Player Heights", x = "Height Bin", y = "Frequency") +
  theme_minimal()

# Step 22: Chi-Square independence test for nationalities and positions of the players

# Clean the data by removing rows with non-finite values for Pos or Nat
cleaned_data <- data %>%
  filter(!is.na(Pos) & !is.na(Nat))

# Create a contingency table of Pos and Nat
contingency_table <- table(cleaned_data$Pos, cleaned_data$Nat)

# Perform Fisher's Exact Test for independence with simulation
fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)

# Print Fisher's Exact Test results
cat("Fisher's Exact Test p-value (with simulation): ", fisher_test$p.value, "\n")

if (fisher_test$p.value < 0.05) {
  cat("Reject the null hypothesis: There is a significant association between position and nationality.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no significant association between position and nationality.\n")
}

# Additional: Visualize the contingency table
contingency_df <- as.data.frame(as.table(contingency_table))

# Plot the contingency table
ggplot(contingency_df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Contingency Table of Position and Nationality", x = "Position", y = "Frequency", fill = "Nationality") +
  theme_minimal()

# Step 23: Standart Error of the mean

# Calculate the mean and standard error of PTS
mean_pts <- mean(cleaned_data$PTS)
std_dev_pts <- sd(cleaned_data$PTS)
n <- length(cleaned_data$PTS)
standard_error <- std_dev_pts / sqrt(n)

# Print the results
cat("Mean of PTS: ", mean_pts, "\n")
cat("Standard Deviation of PTS: ", std_dev_pts, "\n")
cat("Sample Size (n): ", n, "\n")
cat("Standard Error of the Mean (SEM): ", standard_error, "\n")

# Visualize the distribution of PTS with error bars
ggplot(cleaned_data, aes(x = "", y = PTS)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  labs(title = "Distribution of Points Scored (PTS) with Standard Error", y = "Points Scored (PTS)") +
  geom_errorbar(aes(ymin = mean_pts - standard_error, ymax = mean_pts + standard_error),
                width = 0.2, color = "red", linewidth = 1) +
  geom_point(data = cleaned_data[1, ], aes(x = "", y = mean_pts), color = "red", size = 3)

# Step 24: Multiple Regression

# Perform multiple regression analysis
multiple_regression_model <- lm(PTS ~ Cm + Age, data = cleaned_data)

# Summarize the multiple regression model
summary(multiple_regression_model)

# Interpret the results

# Coefficients
coefficients <- coef(multiple_regression_model)
cat("Intercept (β0): ", coefficients[1], "\n")
cat("Coefficient for Cm (β1): ", coefficients[2], "\n")
cat("Coefficient for Age (β2): ", coefficients[3], "\n")

# R-squared value
cat("R-squared value: ", summary(multiple_regression_model)$r.squared, "\n")

# Visualize the multiple regression model (if applicable)
# For example, you can create scatterplots with regression lines for Cm and Age against PTS using ggplot2

# Scatter plot for Cm and PTS
library(ggplot2)
ggplot(cleaned_data, aes(x = Cm, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot with Regression Line (Cm vs PTS)", x = "Height (Cm)", y = "Points Scored (PTS)")

# Step 25: Z-test

# Sample mean
sample_mean <- mean(basketball_data$PTS)

# Sample standard deviation
sample_sd <- sd(basketball_data$PTS)

# Population mean
mu <- 13.5441237113

# Sample size
n <- length(basketball_data$PTS)

# Z-test statistic
z_stat <- (sample_mean - mu) / (sample_sd / sqrt(n))

# P-value (two-tailed)
p_value <- 2 * (1 - pnorm(abs(z_stat)))

# Print results
cat("Z-test statistic: ", z_stat, "\n")
cat("P-value: ", p_value, "\n")

if (p_value < 0.05) {
  cat("Reject the null hypothesis: The sample mean is significantly different from 13.5441237113.\n")
} else {
  cat("Fail to reject the null hypothesis: The sample mean is not significantly different from 13.5441237113.\n")
}

# Step 26: One-sample t-test

# Known mean value to test against
known_mean <- 13.5441237113

# Perform one-sample t-test
t_test_result <- t.test(cleaned_data$PTS, mu = known_mean)

# Print the t-test result
print(t_test_result)

# Plot the results of the t-test
# Create a density plot with the known mean marked

# Calculate the density of PTS
density_pts <- density(cleaned_data$PTS)

# Create a data frame for plotting
density_df <- data.frame(x = density_pts$x, y = density_pts$y)

# Plot the density of PTS and mark the known mean
ggplot(density_df, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_vline(aes(xintercept = known_mean), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(cleaned_data$PTS)), color = "green", linetype = "dotted") +
  labs(title = "Density Plot of Player Points (PTS) with Known Mean",
       x = "Points Scored (PTS)",
       y = "Density") +
  annotate("text", x = known_mean, y = max(density_pts$y) * 0.8, 
           label = paste("Known mean =", round(known_mean, 2)), color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = mean(cleaned_data$PTS), y = max(density_pts$y) * 0.9, 
           label = paste("Sample mean =", round(mean(cleaned_data$PTS), 2)), color = "green", angle = 90, vjust = -0.5)

#

# Step 27: Implement hypothesis testing
# Define a function to plot and perform hypothesis testing
perform_hypothesis_test <- function(data, variable, mu, test_type, test_side, graph_title, x_label) {
  if (test_side == "two.sided") {
    t_test_result <- t.test(data[[variable]], mu = mu, alternative = test_side)
  } else if (test_side == "greater") {
    t_test_result <- t.test(data[[variable]], mu = mu, alternative = "greater")
  } else if (test_side == "less") {
    t_test_result <- t.test(data[[variable]], mu = mu, alternative = "less")
  }
  
  # Plot the density plot with the known mean marked
  density_var <- density(data[[variable]])
  density_df <- data.frame(x = density_var$x, y = density_var$y)
  
  plot <- ggplot(density_df, aes(x = x, y = y)) +
    geom_line(color = "blue") +
    geom_vline(aes(xintercept = mu), color = "red", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(data[[variable]])), color = "green", linetype = "dotted") +
    labs(title = graph_title, x = x_label, y = "Density") +
    annotate("text", x = mu, y = max(density_var$y) * 0.8, 
             label = paste("Known mean =", round(mu, 2)), color = "red", angle = 90, vjust = -0.5) +
    annotate("text", x = mean(data[[variable]]), y = max(density_var$y) * 0.9, 
             label = paste("Sample mean =", round(mean(data[[variable]]), 2)), color = "green", angle = 90, vjust = -0.5)
  
  return(list(t_test_result = t_test_result, plot = plot))
}

# Hypothesis tests for Points (PTS)
pts_two_sided <- perform_hypothesis_test(cleaned_data, "PTS", mu = 13.5, test_type = "t-test", test_side = "two.sided", 
                                         graph_title = "Two-Tailed Test for Points (PTS)", x_label = "Points (PTS)")
pts_right_tailed <- perform_hypothesis_test(cleaned_data, "PTS", mu = 13.5, test_type = "t-test", test_side = "greater", 
                                            graph_title = "Right-Tailed Test for Points (PTS)", x_label = "Points (PTS)")
pts_left_tailed <- perform_hypothesis_test(cleaned_data, "PTS", mu = 13.5, test_type = "t-test", test_side = "less", 
                                           graph_title = "Left-Tailed Test for Points (PTS)", x_label = "Points (PTS)")

# Hypothesis tests for Age
age_two_sided <- perform_hypothesis_test(cleaned_data, "Age", mu = 27.7, test_type = "t-test", test_side = "two.sided", 
                                         graph_title = "Two-Tailed Test for Age", x_label = "Age")
age_right_tailed <- perform_hypothesis_test(cleaned_data, "Age", mu = 27.7, test_type = "t-test", test_side = "greater", 
                                            graph_title = "Right-Tailed Test for Age", x_label = "Age")
age_left_tailed <- perform_hypothesis_test(cleaned_data, "Age", mu = 27.7, test_type = "t-test", test_side = "less", 
                                           graph_title = "Left-Tailed Test for Age", x_label = "Age")

# Hypothesis tests for Height (Cm)
height_two_sided <- perform_hypothesis_test(cleaned_data, "Cm", mu = 199, test_type = "t-test", test_side = "two.sided", 
                                            graph_title = "Two-Tailed Test for Height (Cm)", x_label = "Height (Cm)")
height_right_tailed <- perform_hypothesis_test(cleaned_data, "Cm", mu = 199, test_type = "t-test", test_side = "greater", 
                                               graph_title = "Right-Tailed Test for Height (Cm)", x_label = "Height (Cm)")
height_left_tailed <- perform_hypothesis_test(cleaned_data, "Cm", mu = 199, test_type = "t-test", test_side = "less", 
                                              graph_title = "Left-Tailed Test for Height (Cm)", x_label = "Height (Cm)")

# Print the t-test results
cat("Two-Tailed Test for Points (PTS):\n")
print(pts_two_sided$t_test_result)

cat("\nRight-Tailed Test for Points (PTS):\n")
print(pts_right_tailed$t_test_result)

cat("\nLeft-Tailed Test for Points (PTS):\n")
print(pts_left_tailed$t_test_result)

cat("\nTwo-Tailed Test for Age:\n")
print(age_two_sided$t_test_result)

cat("\nRight-Tailed Test for Age:\n")
print(age_right_tailed$t_test_result)

cat("\nLeft-Tailed Test for Age:\n")
print(age_left_tailed$t_test_result)

cat("\nTwo-Tailed Test for Height (Cm):\n")
print(height_two_sided$t_test_result)

cat("\nRight-Tailed Test for Height (Cm):\n")
print(height_right_tailed$t_test_result)

cat("\nLeft-Tailed Test for Height (Cm):\n")
print(height_left_tailed$t_test_result)

# Plot the results
plot_list <- list(pts_two_sided$plot, pts_right_tailed$plot, pts_left_tailed$plot,
                  age_two_sided$plot, age_right_tailed$plot, age_left_tailed$plot,
                  height_two_sided$plot, height_right_tailed$plot, height_left_tailed$plot)

# Display the plots
do.call("grid.arrange", c(plot_list, ncol = 3))

