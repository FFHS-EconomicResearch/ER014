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

## Durchschnittsalter aller Teilnehmer - mean()

## durchschnittliche Arbeitsstunden nach Geschlecht

