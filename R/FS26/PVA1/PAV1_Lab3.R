# =============================================================================
# LAB 3 | CRISP-DM: Evaluation & Deployment — Performance Mining
# -----------------------------------------------------------------------------
# Lernziele:
#   - Multiples Modell anwenden (aus Dozenten-Demo)
#   - Residuen als "Performance Gap" interpretieren
#   - Underperformer-Ranking erstellen ("Buy List")
#   - Modell-Evaluation: Reicht R² für eine Entscheidung?
# =============================================================================


# -----------------------------------------------------------------------------
# 3.1 | Multiples Modell (Dozenten-Demo nachbauen)
# -----------------------------------------------------------------------------

# Ceteris-Paribus-Logik:
# β₁ = Effekt von Price, wenn Advertising und ShelveLoc konstant gehalten werden

modell_multipel <- lm(
  Sales ~ Price + Advertising + ShelveLoc,
  data = Carseats
)

# Ergebnistabelle
get_regression_table(modell_multipel)

# ┌──────────────────────────────────────────────────────────────────────────┐
# │ INTERPRETATION (Log-Level bei log(Sales) — hier: Level-Level):           │
# │                                                                          │
# │ Price:           β = ___ → 1 USD mehr Preis → ___ Tsd. weniger Sales    │
# │ Advertising:     β = ___ → 1 Tsd. USD mehr Werbung → ___ Tsd. mehr     │
# │ ShelveLocGood:   β = ___ → Guter vs. Bad-Regalplatz → ___ Tsd. mehr    │
# │ ShelveLocMedium: β = ___ → Medium vs. Bad → ___ Tsd. mehr              │
# │                                                                          │
# │ Referenzlevel: ShelveLoc = "Bad" (steckt im Intercept!)                 │
# └──────────────────────────────────────────────────────────────────────────┘

# Modellgüte: Vergleich Einfach vs. Multipel
cat("=== Einfachregression: Sales ~ Price ===\n")
get_regression_summaries(modell_einfach)

cat("\n=== Multiple Regression: Sales ~ Price + Advertising + ShelveLoc ===\n")
get_regression_summaries(modell_multipel)

# → R² gestiegen von ___ auf ___. Was erklärt den Sprung?


# -----------------------------------------------------------------------------
# 3.2 | Performance-Gap berechnen: Residuen als Underperformer-Score
# -----------------------------------------------------------------------------

# Vorhersagen und Residuen an Originaldaten anfügen
filial_scored <- get_regression_points(modell_multipel) |>
  mutate(
    ShelveLoc       = Carseats$ShelveLoc,
    Urban           = Carseats$Urban,
    US              = Carseats$US,
    Income          = Carseats$Income,
    CompPrice       = Carseats$CompPrice,
    performance_gap = residual,
    sales_actual    = Sales,
    sales_benchmark = Sales_hat,
    gap_category    = case_when(
      residual < -2.5 ~ "Stark unter Benchmark",
      residual < -1.0 ~ "Leicht unter Benchmark",
      residual <  1.0 ~ "Im Benchmark-Bereich",
      residual <  2.5 ~ "Leicht über Benchmark",
      TRUE            ~ "Stark über Benchmark"
    ) |>
      factor(levels = c("Stark unter Benchmark",
                        "Leicht unter Benchmark",
                        "Im Benchmark-Bereich",
                        "Leicht über Benchmark",
                        "Stark über Benchmark"))
  )

glimpse(filial_scored)



# -----------------------------------------------------------------------------
# 3.3 | Die Underperformer-Liste: Top 3 (schlechteste Filialen)
# -----------------------------------------------------------------------------

cat("\n=== TOP 3 UNDERPERFORMER (stärkstes negatives Residuum) ===\n")

underperformer <- filial_scored |>
  arrange(performance_gap) |>
  select(ID, sales_actual, sales_benchmark,
         performance_gap, ShelveLoc, Advertising,
         CompPrice, Income) |>
  head(3)

print(underperformer)

# Interpretation:
# Filiale mit performance_gap = -4.5:
# → Verkauft 4.500 Einheiten WENIGER als der Markt-Benchmark erwarten würde
# → Laut Modell ist das NICHT durch Preis, Werbung oder Regalplatz erklärbar
# → Was erklärt es dann? → Das ist die Business-Frage für das Memo!


# -----------------------------------------------------------------------------
# 3.4 | Visualisierung: Performance-Gap Dashboard
# -----------------------------------------------------------------------------

# Plot 1: Tatsächlicher vs. vorhergesagter Umsatz
p3_scatter <- ggplot(filial_scored,
                     aes(x = sales_benchmark, y = sales_actual)) +
  geom_abline(slope = 1, intercept = 0,
              color = "#686868", linewidth = 1,
              linetype = "dashed") +
  geom_point(aes(color = gap_category), alpha = 0.55, size = 1.8) +
  # Top 3 Underperformer beschriften
  geom_point(data = filter(filial_scored,
                           performance_gap %in% sort(performance_gap)[1:3]),
             color = "#D50006", size = 3.5) +
  scale_color_manual(values = c(
    "Stark unter Benchmark"    = "#D50006",
    "Leicht unter Benchmark"   = "#ff8c8c",
    "Im Benchmark-Bereich"     = "#b3b2b2",
    "Leicht über Benchmark"    = "#9b7fc0",
    "Stark über Benchmark"     = "#502479"
  )) +
  labs(title   = "Tatsächlicher vs. Benchmark-Umsatz",
       subtitle = "Punkte unterhalb der Linie = Underperformer",
       x = "Modell-Benchmark (Sales_hat)",
       y = "Tatsächlicher Umsatz (Sales)",
       color = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(color = "#D50006", face = "bold"),
        legend.position = "bottom",
        legend.text   = element_text(size = 8))

# Plot 2: Verteilung der Performance-Gaps
p3_hist <- ggplot(filial_scored, aes(x = performance_gap,
                                     fill = gap_category)) +
  geom_histogram(bins = 35, color = "white", alpha = 0.9) +
  geom_vline(xintercept = 0, color = "#333333",
             linewidth = 1, linetype = "dashed") +
  scale_fill_manual(values = c(
    "Stark unter Benchmark"    = "#D50006",
    "Leicht unter Benchmark"   = "#ff8c8c",
    "Im Benchmark-Bereich"     = "#b3b2b2",
    "Leicht über Benchmark"    = "#9b7fc0",
    "Stark über Benchmark"     = "#502479"
  )) +
  labs(title   = "Verteilung der Performance-Gaps",
       subtitle = "Links von 0 = Underperformer | Rechts = Outperformer",
       x = "Performance Gap (Residuum)",
       y = "Anzahl Filialen",
       fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(color = "#502479", face = "bold"),
        legend.position = "none")

p3_scatter + p3_hist


# Plot 3: Performance-Gap nach ShelveLoc (Confounder noch relevant?)
ggplot(filial_scored,
       aes(x = ShelveLoc, y = performance_gap, fill = ShelveLoc)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#D50006") +
  geom_hline(yintercept = 0, color = "#686868",
             linewidth = 0.8, linetype = "dashed") +
  scale_fill_manual(values = c("Bad"    = "#D50006",
                               "Medium" = "#b3b2b2",
                               "Good"   = "#502479")) +
  labs(title   = "Performance-Gap nach ShelveLoc",
       subtitle = "Im multiplen Modell: kein systematisches Muster mehr → Confounder kontrolliert!",
       x = NULL, y = "Performance Gap") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(color = "#D50006", face = "bold"))


# -----------------------------------------------------------------------------
# 3.5 | Modell-Evaluation: Reicht R² für eine Entscheidung?
# -----------------------------------------------------------------------------

r2 <- get_regression_summaries(modell_multipel)$r_squared
rmse <- get_regression_summaries(modell_multipel)$rmse

cat(sprintf("\nModell-Güte:\n"))
cat(sprintf("  R²   = %.3f → Modell erklärt %.1f%% der Umsatz-Varianz\n",
            r2, r2 * 100))
cat(sprintf("  RMSE = %.3f Tsd. Einheiten → Ø Vorhersagefehler\n", rmse))
cat(sprintf("  Unerklärte Varianz: %.1f%%\n\n", (1 - r2) * 100))


# -----------------------------------------------------------------------------
# 3.6 | Gruppenarbeit: Das Managerial Memo
# -----------------------------------------------------------------------------

# ┌──────────────────────────────────────────────────────────────────────────┐
# │  GRUPPENAUFGABE (~15 Min., 3–4 Personen)                                 │
# │  Schreiben Sie ein kurzes Memo an den Geschäftsführer.                   │
# │                                                                          │
# │  FRAGE 1 — Evaluation:                                                   │
# │  Unser R² = X%. Würden Sie empfehlen, auf Basis dieses Modells           │
# │  Filialen zu schließen? Begründen Sie mit dem RMSE.                      │
# │                                                                          │
# │  FRAGE 2 — Omitted Variable Bias:                                        │
# │  Welche Faktoren fehlen in unseren Daten?                               │
# │  (Demografie, Wettbewerb, Management-Qualität, Standort-Frequenz, ...)   │
# │  Warum verzerren fehlende Variablen ALLE Koeffizienten?                  │
# │                                                                          │
# │  FRAGE 3 — Data Strategy (Make or Buy):                                  │
# │  Option A: Geodaten + Demografie von Anbieter kaufen (Commodity)         │
# │  Option B: Eigene Kundenbefragung / Loyalty-Card aufbauen (Proprietary)  │
# │                                                                          │
# │  Wann lohnt sich der Dateneinkauf?                                       │
# │  Expected Value Framework:                                               │
# │    E[ΔGewinn] > Kosten Daten → kaufen!                                   │
# │                                                                          │
# │  FRAGE 4 (Bonus):                                                        │
# │  Was würde passieren, wenn wir ALLE 11 Variablen ins Modell werfen?      │
# │  → Stichwort: Overfitting / False Discoveries (Taddy, 2019)             │
# └──────────────────────────────────────────────────────────────────────────┘

# Bonus: Alle Variablen im Modell (was passiert mit R²?)
modell_voll <- lm(Sales ~ ., data = Carseats)
get_regression_summaries(modell_voll)
# → R² steigt — aber ist das besser? Diskussion!


# =============================================================================
# SYNTHESE: CRISP-DM Zyklus 1 vollständig
# -----------------------------------------------------------------------------
#
#  Phase 1 – Business Understanding:
#    "Underperformer finden" → negatives Residuum als Benchmark-Abweichung
#
#  Phase 2 – Data Understanding (Lab 1):
#    Sales leicht rechtsschief | ShelveLoc = wichtiger Confounder
#    Preis negativ korreliert mit Sales (Nachfragegesetz)
#
#  Phase 3 – Data Preparation:
#    log(Sales) als Transformation | ShelveLoc als Faktor → Dummies
#
#  Phase 4 – Modeling (Lab 2 + Demo):
#    Einfachregression R² ≈ 0.15 → Underfitting
#    Multiple Regression R² ≈ 0.50+ → Confounder kontrolliert
#
#  Phase 5 – Evaluation (Lab 3):
#    R² reicht NICHT für Schließungsentscheidungen
#    RMSE = Ø Fehler in Tsd. Einheiten → Praktische Relevanz!
#
#  Phase 6 – Deployment:
#    Managerial Memo: Make or Buy? Nächster CRISP-DM-Zyklus mit mehr Daten
#
# → Ausblick Block 2: Wie sicher sind unsere Koeffizienten wirklich?
#   Bootstrapping, Konfidenzintervalle, Hypothesentests
# =============================================================================

