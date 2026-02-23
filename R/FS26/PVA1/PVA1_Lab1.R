# ==============================================================================
# STRATEGIC DATA SCIENCE: BLOCK 1 (CRISP-DM CYCLE 1)
# Szenario: "Real Estate Arbitrage" (Immobilienbewertung & Value Mining)
# ==============================================================================

# Pakete laden (wir brauchen heute keine externen Dateien!)
library(tidyverse)
library(moderndive)

# ------------------------------------------------------------------------------
# SETUP: DATEN LADEN
# Datensatz 'house_prices' aus King County, Washington (enthält Hausverkäufe)
# ------------------------------------------------------------------------------
tbl_RE <- moderndive::house_prices

# Inspektion der Rohdaten (Data Understanding)
glimpse(tbl_RE)

# ------------------------------------------------------------------------------
# LAB 1: EXPLORATIVE DATENANALYSE (EDA) & TRANSFORMATION
# ------------------------------------------------------------------------------

# 1. Histogramm der Immobilienpreise
tbl_RE %>%
  ggplot( aes(x = price)) +
    geom_histogram(fill = "steelblue", color = "black", bins = 50) +
    scale_x_continuous(labels = scales::dollar_format()) +
    theme_minimal() +
    labs(title = "Verteilung der Hauspreise",
         x = "Preis ($)", y = "Anzahl")
# -> Erkenntnis: Stark rechtsschief. Typisch für "Winner-takes-all" / Asset-Märkte.

# 2. Ökonomische Transformation (nach Taddy: Multiplikative Prozesse)
# Immobilienpreise ändern sich prozentual (z.B. "10% Wertsteigerung durch Renovierung")
tbl_RE <- tbl_RE %>%
            mutate(log_price = log(price))

# Verteilung prüfen
tbl_RE %>%
  ggplot(aes(x = log_price)) +
    geom_histogram(fill = "darkgreen", color = "black", bins = 50) +
    theme_minimal() +
    labs(title = "Verteilung logarithmierter Hauspreis", x = "Log(Preis)")

# 3. Bivariater Zusammenhang: Wohnfläche (sqft_living) vs. Preis
tbl_RE %>%
  ggplot(aes(x = sqft_living, y = log_price)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", color = "red", se=FALSE) +
    theme_minimal() +
    labs(title = "Werttreiber Wohnfläche",
         x = "Wohnfläche (sqft)", y = "Log(Preis)")
