library(tidyverse)
library(glue)
library(readxl)

# Download data -----
#date <-  Sys.Date()
#my_URL <- "https://archive.ics.uci.edu/static/public/477/real+estate+valuation+data+set.zip"
#download.file(url=my_URL,
#              destfile=xfun::from_root("data","raw",glue('REprices_{date}.zip')),
#              method='auto')


# Import data -----
date <- "2024-04-13"
tbl_prices <- read_excel(unzip(xfun::from_root("data", "raw", "REprices_2024-04-13.zip"),
                               "Real estate valuation data set.xlsx"))


# Plot data -----

## Scatterplot-Matrix ----
library(GGally)

tbl_prices %>%
    select(-1) %>%
    ggpairs()
