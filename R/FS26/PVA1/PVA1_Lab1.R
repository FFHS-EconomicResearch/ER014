# =============================================================================
# ER014 – Data Science and Strategy for Business
# Block 1 | Labs 1–3: Carseats Fallstudie
# -----------------------------------------------------------------------------
# Szenario:
#   Sie sind das Data-Science-Team einer Handelskette mit 400 Filialen.
#   Produkt: Kindersitze (Carseats). Auftrag des Managements:
#   "Finde Underperformer-Filialen und bewerte unser Werbebudget!"
#
# Datensatz: ISLR2::Carseats
#   400 Filialen, 11 Variablen — kein Import nötig, läuft überall
#
# Pakete: tidyverse, ISLR2, moderndive, skimr, patchwork
# =============================================================================


# -----------------------------------------------------------------------------
# 0 | Setup — Pakete installieren & laden
# -----------------------------------------------------------------------------

# Einmalig ausführen falls Pakete fehlen:
# install.packages(c("tidyverse", "ISLR2", "moderndive", "skimr", "patchwork"))

library(tidyverse)
library(ISLR2)
library(moderndive)
library(skimr)
library(patchwork)

# Daten laden
data(Carseats)

# Wenn glimpse(Carseats) 400 Zeilen und 11 Spalten zeigt: Ready!
glimpse(Carseats)


# =============================================================================
# LAB 1 | CRISP-DM: Data Understanding — Explorative Datenanalyse
# -----------------------------------------------------------------------------
# Lernziele:
#   - Datensatz strukturell kennenlernen
#   - Verteilungen und Ausreißer erkennen
#   - Bivariate Zusammenhänge visualisieren
#   - Taddy-Logik: Warum logarithmieren?
# =============================================================================


# -----------------------------------------------------------------------------
# 1.1 | Datensatz kennenlernen
# -----------------------------------------------------------------------------

# Struktur: Variablentypen, erste Werte
glimpse(Carseats)

# Erweiterte deskriptive Statistiken (inkl. Histogramme im Viewer)
skim(Carseats)

# Kategoriale Variablen: Ausprägungen prüfen
Carseats |> count(ShelveLoc)   # Regalplatz-Qualität
Carseats |> count(Urban)        # Stadtlage?
Carseats |> count(US)           # US-Filiale?

# ┌──────────────────────────────────────────────────────────────────────────┐
# │ FRAGEN AN DIE GRUPPE:                                                    │
# │ - Wie viele Variablen sind numerisch, wie viele kategorisch?             │
# │ - Was bedeutet Sales? In welcher Einheit?                                │
# │ - Welche Variable ist unsere Zielvariable?                               │
# └──────────────────────────────────────────────────────────────────────────┘


# -----------------------------------------------------------------------------
# 1.2 | Verteilung des Umsatzes (Zielvariable)
# -----------------------------------------------------------------------------

# Histogramm: Rohdaten
p1_hist_raw <- ggplot(Carseats, aes(x = Sales)) +
  geom_histogram(bins = 30, fill = "#D50006",
                 color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean(Carseats$Sales),
             color = "#502479", linewidth = 1.2,
             linetype = "dashed") +
  geom_vline(xintercept = median(Carseats$Sales),
             color = "#7d0a52", linewidth = 1.2,
             linetype = "dotted") +
  annotate("text",
           x = mean(Carseats$Sales) + 0.4,
           y = 38,
           label = paste0("Ø ", round(mean(Carseats$Sales), 1)),
           color = "#502479", size = 3.2, hjust = 0, fontface = "bold") +
  annotate("text",
           x = median(Carseats$Sales) - 0.4,
           y = 34,
           label = paste0("Med. ", round(median(Carseats$Sales), 1)),
           color = "#7d0a52", size = 3.2, hjust = 1, fontface = "bold") +
  labs(title   = "Verteilung Sales — Rohdaten",
       subtitle = "Gestrichelt = Mittelwert | Gepunktet = Median",
       x = "Sales (Tsd. Einheiten)", y = "Anzahl Filialen") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(color = "#D50006", face = "bold"))

p1_hist_raw

# Kennzahlen: Schiefe diagnostizieren
Carseats |>
  summarise(
    mean_sales   = mean(Sales),
    median_sales = median(Sales),
    sd_sales     = sd(Sales),
    skewness     = (mean(Sales) - median(Sales)) / sd(Sales),  # Pearson-Schiefe
    min_sales    = min(Sales),
    max_sales    = max(Sales)
  )

# ┌──────────────────────────────────────────────────────────────────────────┐
# │ TADDY-LOGIK (Debriefing Folie 12):                                       │
# │                                                                          │
# │ Umsatzdaten folgen oft multiplikativen Prozessen:                        │
# │   Sales_i = α · Price_i^β₁ · Adv_i^β₂ · ε_i                            │
# │                                                                          │
# │ → OLS auf Rohdaten: Heteroskedastizität! Residuen nicht normalverteilt.  │
# │ → Logarithmierung konvertiert multiplikative in additive Struktur.       │
# │ → Koeffizienten werden zu Elastizitäten (%-Interpretation).              │
# └──────────────────────────────────────────────────────────────────────────┘

# Log-Transformation erstellen
Carseats <- Carseats |>
  mutate(log_Sales = log(Sales))

# Vergleich: Roh vs. log-transformiert
p1_hist_log <- ggplot(Carseats, aes(x = log_Sales)) +
  geom_histogram(bins = 30, fill = "#502479",
                 color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean(Carseats$log_Sales),
             color = "#D50006", linewidth = 1.2,
             linetype = "dashed") +
  labs(title   = "Verteilung log(Sales) — transformiert",
       subtitle = "Annähernd symmetrisch nach Log-Transformation",
       x = "log(Sales)", y = "Anzahl Filialen") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(color = "#502479", face = "bold"))

# Beide nebeneinander
p1_hist_raw + p1_hist_log +
  plot_annotation(
    title   = "Taddy-Logik: Warum logarithmieren?",
    caption = "Quelle: ISLR2::Carseats"
  )


# -----------------------------------------------------------------------------
# 1.3 | Bivariate Analyse: Preis vs. Umsatz
# -----------------------------------------------------------------------------

p1_scatter <- ggplot(Carseats, aes(x = Price, y = Sales)) +
  geom_point(alpha = 0.4, color = "#7d0a52", size = 1.8) +
  geom_smooth(method = "lm",    color = "#D50006",
              se = FALSE, linewidth = 1.2) +
  geom_smooth(method = "loess", color = "#502479",
              se = FALSE, linewidth = 1, linetype = "dashed") +
  labs(title   = "Preis vs. Umsatz",
       subtitle = "Rot = linearer Trend | Gestrichelt = LOESS (flexibel)",
       caption  = "Wenn Linien abweichen: linearer Zusammenhang fragwürdig!",
       x = "Preis (USD)", y = "Sales (Tsd. Einheiten)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(color = "#D50006", face = "bold"))

p1_scatter

# Ergänzung: Sales nach Regalplatz-Qualität
p1_boxplot <- ggplot(Carseats, aes(x = ShelveLoc, y = Sales,
                                   fill = ShelveLoc)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#D50006",
               outlier.size = 1.5) +
  scale_fill_manual(values = c("Bad"    = "#D50006",
                               "Medium" = "#b3b2b2",
                               "Good"   = "#502479")) +
  labs(title   = "Sales nach Regalplatz-Qualität",
       subtitle = "ShelveLoc als zentraler Confounder",
       x = NULL, y = "Sales (Tsd. Einheiten)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(color = "#D50006", face = "bold"))

p1_boxplot

# ┌──────────────────────────────────────────────────────────────────────────┐
# │ FRAGEN AN DIE GRUPPE (Lab 1 Debrief):                                    │
# │ - Ist Sales normalverteilt? Was sagt der Vergleich Mean vs. Median?      │
# │ - Wie stark ist der Preis-Effekt visuell?                                │
# │ - Was fällt beim ShelveLoc-Boxplot auf?                                  │
# │   → Starke Unterschiede = wichtige Variable für das Modell!              │
# └──────────────────────────────────────────────────────────────────────────┘


# -----------------------------------------------------------------------------
# 1.4 | Korrelationsstruktur
# -----------------------------------------------------------------------------

Carseats |>
  select(Sales, Price, Advertising, Income,
         Age, Population, Education) |>
  cor() |>
  round(3)

# Welche Variable korreliert am stärksten mit Sales?
# Was bedeutet das für unser Modell?


