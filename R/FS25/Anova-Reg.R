# Load required packages
library(openintro)
library(ggplot2)

# Load the mariokart data
data(mariokart)

# Examine the structure of the dataset
str(mariokart)

# For this demonstration, we'll use 'cond' (condition: new or used)
# as our categorical predictor and 'total_pr' (total price) as our response variable

# Create boxplot to visualize the relationship
ggplot(mariokart, aes(x = cond, y = total_pr)) +
  geom_boxplot() +
  labs(title = "Total Price by Condition",
       x = "Condition",
       y = "Total Price ($)") +
  theme_minimal()

# Method 1: ANOVA
anova_model <- aov(total_pr ~ cond, data = mariokart)
summary(anova_model)

# Method 2: Linear Regression
lm_model <- lm(total_pr ~ cond, data = mariokart)
summary(lm_model)

# Extract the F-statistic from both approaches
anova_f <- summary(anova_model)[[1]]["F value"][1,1]
lm_f <- summary(lm_model)$fstatistic[1]
cat("ANOVA F-statistic:", anova_f, "\n")
cat("Linear Regression F-statistic:", lm_f, "\n")

# Extract p-values from both approaches
anova_p <- summary(anova_model)[[1]]["Pr(>F)"][1,1]
lm_p <- pf(summary(lm_model)$fstatistic[1],
           summary(lm_model)$fstatistic[2],
           summary(lm_model)$fstatistic[3],
           lower.tail = FALSE)
cat("ANOVA p-value:", anova_p, "\n")
cat("Linear Regression p-value:", lm_p, "\n")

# Extract sum of squares from ANOVA
anova_table <- summary(anova_model)[[1]]
cat("ANOVA Sum of Squares for condition:", anova_table["Sum Sq"][1,1], "\n")
cat("ANOVA Residual Sum of Squares:", anova_table["Sum Sq"][2,1], "\n")

# Extract sum of squares from regression model
lm_anova <- anova(lm_model)
cat("Linear Regression Model Sum of Squares:", lm_anova["Sum Sq"][1,1], "\n")
cat("Linear Regression Residual Sum of Squares:", lm_anova["Sum Sq"][2,1], "\n")

# Demonstrate that regression coefficients correspond to group means
group_means <- aggregate(total_pr ~ cond, data = mariokart, mean)
print(group_means)

# The intercept in the regression model corresponds to the reference level (typically the first alphabetically)
# And the coefficient for the second level represents the difference between the two means
cat("Regression intercept (mean of first level):", coef(lm_model)[1], "\n")
cat("Regression coefficient (difference between means):", coef(lm_model)[2], "\n")
cat("Calculated difference between means:", group_means$total_pr[2] - group_means$total_pr[1], "\n")
