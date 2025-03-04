library(tidyverse)
#install.packages("infer") #  ggf. Paket installieren
library(infer)

# GSS-Daten im tibble-Objekt tbl_gss speichern
tbl_gss <- gss


# Aufgaben

## Anzahl der Teilnehmer
length(tbl_gss$year)
fallzahl <- tbl_gss %>%
                summarise(Anzahl=n())
fallzahl

## Durchschnittsalter aller Teilnehmer - mean()
altersdurchschnitt <- tbl_gss %>%
                            summarise(mean_age = mean(age))
altersdurchschnitt

## durchschnittliche Arbeitsstunden nach Geschlecht
stunden <- tbl_gss %>%
              group_by(sex) %>%
              summarise(mean_hours = mean(hours))
stunden
