# Create Data ------
library(tidyverse)
set.seed(23)
hours_wp <- 60-round(rnorm(12,10,5),0)
hours_ho <- 60-round(rnorm(12,20,5),0)

tbl_time <- tibble(number=c(1:length(hours_wp)),office=hours_wp,homeoffice=hours_ho)
tbl_time

tbl_long <- tbl_time %>%
                pivot_longer(-number,names_to = "type",values_to = "minutes")
tbl_long


## independent -----
tbl_ind <- tbl_long %>%
              group_by(type) %>%
              mutate(number=if_else(type=="office",number,number+length(hours_wp)))

## descriptive statistics -----
tbl_long %>%
        group_by(type) %>%
        summarise(av=mean(minutes))


# Dataviz ----

## Boxplot dependent ----
tbl_long %>%
    ggplot(aes(x=type,y=minutes)) +
      geom_boxplot()

## kernel density dependent ----
tbl_long %>%
  ggplot(aes(x=minutes,color=type)) +
   geom_density()

# Tests ----

library(infer)

tbl_long %>%
  specify(minutes ~ type) %>%
  hypothesize(null="independence")









# large sample -----
set.seed(23)
hours_wp <- 60-round(rnorm(100,10,5),0)
hours_ho <- 60-round(rnorm(100,20,5),0)

tbl_time <- tibble(number=c(1:length(hours_wp)),office=hours_wp,homeoffice=hours_ho)
tbl_time

tbl_long <- tbl_time %>%
                pivot_longer(-number,names_to = "type",values_to = "minutes")
tbl_long

## kernel density dependent ----
tbl_long %>%
  ggplot(aes(x=minutes,color=type)) +
  geom_density(adjust=2)

?geom_density




tbl_grid <- tibble(x=seq(-100,100,.5))
tbl_grid %>%
  ggplot(aes(x=x)) + stat_density(kernel = "gaussian")



# two-sample t-Test ----

#https://ismayc.github.io/teaching/sample_problems/two-means-indep.html




tbl_cleSac <- read_tsv("http://ismayc.github.io/teaching/sample_problems/cleSac.txt")

tbl_cleSac %>%
  write_rds(xfun::from_root("data","raw","cleSac_(Ismay).rds"))

tbl_cleSac <- read_rds(xfun::from_root("data","raw","cleSac_(Ismay).rds"))

tbl_cleSac %>%
    rename(metro_area = Metropolitan_area_Detailed,
          income = Total_personal_income) %>%
    na.omit()


