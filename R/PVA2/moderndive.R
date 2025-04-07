library(tidyverse)

p <- ggplot(mpg, aes(x = drv, y = hwy)) + geom_point() +
                  geom_categorical_model()
p
# In the above visualization, the solid line corresponds to the mean of 19.2
# for the baseline group "4", whereas the dashed lines correspond to the
# means of 28.19 and 21.02 for the non-baseline groups "f" and "r" respectively.
# In the corresponding regression table however the coefficients for "f" and "r"
# are presented as offsets from the mean for "4":
model <- lm(hwy ~ drv, data = mpg)
get_regression_table(model)
# You can use different colors for each categorical level
p %+% aes(color = drv)
# But mapping the color aesthetic doesn't change the model that is fit
p %+% aes(color = class)


# parallel slopes ----

ggplot(evals, aes(x = age, y = score, color = ethnicity)) + geom_point() +
  geom_parallel_slopes(se = FALSE)
# Basic usage
ggplot(evals, aes(x = age, y = score, color = ethnicity)) + geom_point() +
  geom_parallel_slopes()
ggplot(evals, aes(x = age, y = score, color = ethnicity)) + geom_point() +
  geom_parallel_slopes(se = FALSE)
# Supply custom aesthetics
ggplot(evals, aes(x = age, y = score, color = ethnicity)) + geom_point() +
  geom_parallel_slopes(se = FALSE, size = 4)


# Tidyverse-friendly Intro Regression ----
library(moderndive)
# Fit regression model:
mpg_model <- lm(mpg ~ hp, data = mtcars)
# Regression tables:
get_regression_table(mpg_model)
# Information on each point in a regression:
get_regression_points(mpg_model)
# Regression summaries
get_regression_summaries(mpg_model)
# Plotting parallel slopes models
ggplot(evals, aes(x = age, y = score, color = ethnicity)) +
  geom_point() + geom_parallel_slopes(se = FALSE)



evals_ch5 <- evals %>%
  select(ID, score, bty_avg, age)
glimpse(evals_ch5)

# Fit regression model:
score_model <- lm(score ~ bty_avg, data = evals_ch5)

reg_points <- get_regression_points(score_model)

score_model %>%
  ggplot(aes(x=bty_avg,y=score)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Residuen vs. vorhergesagte Werte",
       x = "Vorhergesagte Preise", y = "Residuen")
