# Code by Jan Vanhove
#https://janhove.github.io/posts/2019-09-11-collinearity/

# Packages
library(tidyverse)
library(broom)

# Read in the four generated datasets
strong <- read.csv("https://janhove.github.io/datasets/strong_collinearity.csv")
weak <- read.csv("https://janhove.github.io/datasets/weak_collinearity.csv")
none <- read.csv("https://janhove.github.io/datasets/no_collinearity.csv")
nonlinear <- read.csv("https://janhove.github.io/datasets/nonlinearity.csv")

# Load the custom function for drawing scatterplot matrices,
# then drew Figures 1-4
source("https://janhove.github.io/RCode/scatterplot_matrix.R")
scatterplot_matrix(strong[, c(3, 1, 2)])
scatterplot_matrix(weak[, c(3, 1, 2)])
scatterplot_matrix(none[, c(3, 1, 2)])
scatterplot_matrix(nonlinear[, c(3, 1, 2)])

# Fit multiple regression models
strong.lm <- lm(outcome ~ predictor1 + predictor2, data = strong)
weak.lm <- lm(outcome ~ predictor1 + predictor2, data = weak)
none.lm <- lm(outcome ~ predictor1 + predictor2, data = none)
nonlinear.lm <- lm(outcome ~ predictor1 + predictor2, data = nonlinear)

# Extract estimates + 90% CIs
strong_out <- tidy(strong.lm, conf.int = TRUE, conf.level = 0.90) |>
  mutate(dataset = "strong")
weak_out <- tidy(weak.lm, conf.int = TRUE, conf.level = 0.90) |>
  mutate(dataset = "weak")
none_out <- tidy(none.lm, conf.int = TRUE, conf.level = 0.90) |>
  mutate(dataset = "none")
nonlinear_out <- tidy(nonlinear.lm, conf.int = TRUE, conf.level = 0.90) |>
  mutate(dataset = "nonlinear")
outputs <- bind_rows(strong_out, weak_out, none_out, nonlinear_out)

# Draw Figure 5
dummy <- data.frame(term = unique(outputs$term), prm = c(0, 0.4, 1.9))
outputs |>
  ggplot(aes(x = factor(dataset, levels = c("nonlinear", "none",
                                            "weak", "strong")),
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  geom_pointrange() +
  facet_wrap(~ term) +
  geom_hline(data = dummy, aes(yintercept = prm),
             linetype = "dashed") +
  ylab("estimated coefficient with 90% confidence interval") +
  xlab("dataset") +
  coord_flip()

# Function for simulating effect of collinearity on estimates
collinearity <- function(n_sim = 1000, n_sample = 50,
                         rho = 0.90,
                         coefs = c(0.4, 1.9),
                         sd_error = 3.5) {
  # This function generates two correlated
  # predictors and an outcome. It then
  # runs regression models (including ridge regression)
  # on these variables and outputs the estimated
  # regression coefficients for the predictors.
  # It does this a large number of times (n_sim).

  # Package for LASSO/ridge regression
  require("glmnet")

  estimates <- matrix(ncol = 8, nrow = n_sim)

  for (i in 1:n_sim) {
    # Generate correlated predictors
    predictors <- MASS::mvrnorm(
      n = n_sample,
      mu = c(0, 0),
      Sigma = rbind(
        c(1, rho),
        c(rho, 1)
      )
    )

    # Generate outcome
    outcome <- as.vector(coefs %*% t(predictors) + rnorm(n_sample, sd = sd_error))

    # Run multiple regression model
    multiple_regression <- lm(outcome ~ predictors[, 1] + predictors[, 2])

    # Run single regression models
    simple_first <- lm(outcome ~ predictors[, 1])
    simple_second <- lm(outcome ~ predictors[, 2])

    # Ridge regression
    lambda_seq <- 10^seq(2, -2, by = -0.1)
    cv_output <- cv.glmnet(predictors, outcome, nfolds = 10,
                           alpha = 0, lambda = lambda_seq)
    best_lambda <- cv_output$lambda.min
    ridge_model <- glmnet(predictors, outcome, alpha = 0,
                          lambda = best_lambda)

    # Save regression coefficients
    estimated_coefficients <- c(
      coef(multiple_regression)[2:3],
      summary(multiple_regression)$coefficients[2:3, 2],
      coef(simple_first)[2],
      coef(simple_second)[2],
      coef(ridge_model)[2:3]
    )

    estimates[i, ] <- estimated_coefficients
  }

  results <- data.frame(
    multiple_est_pred1 = estimates[, 1],
    multiple_est_pred2 = estimates[, 2],
    multiple_se_pred1 = estimates[, 3],
    multiple_se_pred2 = estimates[, 4],
    simple_est_pred1 = estimates[, 5],
    simple_est_pred2 = estimates[, 6],
    ridge_est_pred1 = estimates[, 7],
    ridge_est_pred2 = estimates[, 8]
  )
  results
}

# Simulate effects of strong collinearity
strong_coll <- collinearity(rho = 0.98)

# Simulate effect of perfect orthogonality (zero collinearity)
no_coll <- collinearity(rho = 0)

# Combine
strong_coll$Collinearity <- "strong collinearity\n(r = 0.98)"
no_coll$Collinearity <- "no collinearity\n(r = 0.00)"
all_data <- bind_rows(strong_coll, no_coll)

# Figure 6
ggplot(all_data,
       aes(x = multiple_est_pred1,
           y = after_stat(density))) +
  geom_histogram(bins = 50, colour = "black", fill = "grey80") +
  facet_wrap(~ Collinearity) +
  geom_vline(xintercept = 0.4, linetype = "dashed", colour = "red") +
  xlab("estimated regression coefficient for first predictor\nin multiple regression models")

# Table 1
map_dfr(list(strong.lm, weak.lm, none.lm, nonlinear.lm), glance) |>
  mutate(Dataset = c("strong", "weak", "none", "nonlinear")) |>
  select(Dataset, `R²` = r.squared, `p-value of overall fit` = p.value) |>
  knitr::kable("html") |>
  kableExtra::kable_styling(full_width = FALSE)

# Figure 7
lexdiv <- read_csv("https://janhove.github.io/datasets/LexicalDiversityFrench.csv")
ratings <- read_csv("https://janhove.github.io/datasets/meanRatingPerText_French.csv")
ratings$Text <- substr(ratings$Text, 15, nchar(ratings$Text))
d <- left_join(ratings, lexdiv, by = c("Text" = "textName"))
scatterplot_matrix(d |> select(meanRating, TTR, nTokens) |>
                     mutate(sqrt_nTokens = log10(nTokens)) |>
                     select(-nTokens),
                   labels = c("mean diversity rating",
                              "type/token ratio",
                              "log10 tokens"))

# Figure 8
ggplot(all_data,
       aes(x = simple_est_pred1,
           y = after_stat(density))) +
  geom_histogram(bins = 50, colour = "black", fill = "grey80") +
  facet_wrap(~ Collinearity) +
  geom_vline(xintercept = 0.4, linetype = "dashed", col = "red") +
  xlab("estimated regression coefficient for first predictor\nin simple regression models")

# Figure 9
ggplot(all_data,
       aes(x = ridge_est_pred1,
           y = after_stat(density))) +
  geom_histogram(bins = 50, colour = "black", fill = "grey80") +
  facet_wrap(~ Collinearity) +
  geom_vline(xintercept = 0.4, linetype = "dashed", col = "red") +
  xlab("estimated regression coefficient for first predictor\nin ridge regression models")
