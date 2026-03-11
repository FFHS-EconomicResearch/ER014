# Daten -----
#Enthalten im Paket AmesHousing
#install.packages("AmesHousing").  #Paket ggf. installieren
## Laden ----
library(AmesHousing) # Datensatz laden

## Bereinigen und als tibble-Objekt speichern -----
library(tidyverse) #tidyverse-Pakete laden
tbl_ames <- ames_raw %>%
              janitor::clean_names() #Funktion clean_names() aus janitor-Paket für einheitlichen Variablenbezeichnung gemäss Konvention


# Dataviz -----
#Abbildungen aus Taddy/Hendrix/Harding (2023, S. 121)

p <- tbl_ames %>% ggplot(aes(sale_price)) +
                            geom_histogram(bins=13)

## Streudiagramm

### Simpel ----
p <- tbl_ames %>% ggplot(aes(lot_area,sale_price)) +
                                    geom_point()
p
### mit logarithmierten Werten -----
p + scale_y_log10() + scale_x_log10()

