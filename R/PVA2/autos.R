library(tidyverse)
library(moderndive)
library(infer)
# Import data -----
my_in_file <- "autos_(StockerUIBK)_20240414.csv"
tbl_autos <- read_csv2(xfun::from_root("data","raw",my_in_file))

# Dataviz ------
p <- tbl_autos %>%
  ggplot(aes(x=Alter,y=Preis)) +
  geom_point() + theme_light() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits=c(0,6),breaks=seq(0, 5, 1)) +
  scale_y_continuous(limits=c(0,26000),breaks=seq(0, 25000, 5000)) +
  labs(x="Alter (in Jahren)",y="Preis",title="Gebrauchtwagen")

p

# Resampling Regression ------

## Ein numerischer Prediktor -----

obs_slope <- tbl_autos %>%
                specify(Preis ~ Alter) %>%
                calculate(stat = "slope")
obs_slope

### Bootstrapping-Verteilung des Koeffizienten ----
boot_dist <-tbl_autos %>%
                specify(Preis~Alter) %>%
                generate(reps = 1000, type = "bootstrap") %>%
                calculate(stat = "slope")
visualise(boot_dist)

### Konfidenzintervall des Koeffizienten ------

#### Methode Perzentile -----
percentile_ci <- boot_dist %>%
                        get_confidence_interval(type = "percentile",
                                                level = 0.95)
percentile_ci


#### Methode Standardfehler -----
se_ci <- boot_dist %>%
            get_confidence_interval(type = "se",
                                    point_estimate = obs_slope,
                                    level = 0.95)
se_ci
p_ci <- visualize(boot_dist) +
            shade_confidence_interval(endpoints = percentile_ci, fill = NULL,
                                      linetype = "solid", color = "dodgerblue2") +
            shade_confidence_interval(endpoints = se_ci, fill = NULL,
                                      linetype = "dashed", color = "dodgerblue3")
p_ci # sehr ähnlich, keines enthält Null. Legt signifikant negativen Koeffizienten nahe

### Hypothesentest des Koeffizienten -----
null_dist <-tbl_autos %>%
              specify(Preis~Alter) %>%
              hypothesize(null="independence") %>% #Nullhypothese als zusätzliches Element der Pipe
              generate(reps = 1000, type = "permute") %>%  # permute statt bootstrap
              calculate(stat = "slope")

# neben "independence" (bivariat) gibt es noch "point" (univariat, unabhängige Stichprobe (independent measures)) und "paired independence" (univariat, abhängige Stichproben (repeated measures)) als Optionen der `hypothesize()`-Funktion.

null_dist %>% visualize() +   # Verteilung um Null! - wg. $H_0:~\beta_1=0$
                  shade_p_value(obs_slope,direction = "less")

null_dist %>%
  get_p_value(obs_stat = obs_slope,direction = "less")





# Traditional Regression ----

## Ein numerischer Regressor -----
reg_auto <- tbl_autos %>%
                lm(Preis~Alter,.)
reg_auto %>%
  get_regression_table()
reg_auto %>%
  get_regression_summaries()


lo_ci <- reg_auto %>%
            get_regression_table() %>%
            select(term,lower_ci) %>%
            filter(term=="Alter") %>%
            pull()
up_ci <- reg_auto %>%
              get_regression_table() %>%
              select(term,upper_ci) %>%
              filter(term=="Alter") %>%
              pull()
trad_ci <- c(lo_ci,up_ci)

p_ci + shade_confidence_interval(endpoints = se_ci, fill = NULL,
                                 linetype = "dotted", color = "#502479")

## Zwei numerische Regressoren -----
reg_auto2 <- tbl_autos %>%
                  lm(Preis~Alter+km,.)
reg_auto2 %>%
  get_regression_table()
reg_auto2 %>%
  get_regression_summaries()

### Multikollinearität ------
tbl_autos %>%
  summarise(r_XY=cor(Alter,km))
car::vif(reg_auto2)

# Diagnostik ----
library(ggfortify)
p_diag <- autoplot(reg_auto)
p_diag
