# =============================================================================
# LAB 2 | CRISP-DM: Data Preparation & Modeling — Baseline-Modell
# -----------------------------------------------------------------------------
# Lernziele:
#   - Einfachregression fitten und interpretieren
#   - get_regression_table() lesen
#   - Residualanalyse: Wo versagt das Modell?
#   - Underfitting und Omitted Variable Bias erkennen
# =============================================================================


# -----------------------------------------------------------------------------
# 2.1 | Einfachregression: Sales ~ Price
# -----------------------------------------------------------------------------

# Das "naive" Baseline-Modell
modell_einfach <- lm(Sales ~ Price, data = Carseats)

# Ergebnistabelle (moderndive-Stil)
get_regression_table(modell_einfach)

# ┌──────────────────────────────────────────────────────────────────────────┐
# │ INTERPRETATIONSSCHEMA:                                                   │
# │                                                                          │
# │ Intercept (β₀): Erwarteter Sales-Wert wenn Price = 0                     │
# │   → Inhaltlich nicht sinnvoll interpretierbar                            │
# │                                                                          │
# │ Price (β₁): ___                                                          │
# │   → 1 USD Preiserhöhung verändert Sales um ___ Tsd. Einheiten           │
# │   → Vorzeichen: Negativ (Nachfragegesetz) — erwartet?                   │
# │                                                                          │
# │ p_value < 0.05 → Koeffizient statistisch signifikant                    │
# │ lower_ci / upper_ci → 95%-Konfidenzintervall                             │
# └──────────────────────────────────────────────────────────────────────────┘

# Modellgüte
get_regression_summaries(modell_einfach)

# R² notieren: ___
# Interpretation: Der Preis allein erklärt ___% der Umsatz-Unterschiede.
# Reicht das für eine Underperformer-Entscheidung?


# -----------------------------------------------------------------------------
# 2.2 | Visualisierung des Baseline-Modells
# -----------------------------------------------------------------------------

p2_fit <- ggplot(Carseats, aes(x = Price, y = Sales)) +
  geom_point(alpha = 0.35, color = "#7d0a52", size = 1.8) +
  geom_smooth(method = "lm", color = "#D50006",
              se = FALSE, linewidth = 1.3) +
  annotate("label",
           x = 80, y = 15,
           label = paste0("R² = ",
                          round(get_regression_summaries(modell_einfach)$r_squared, 3),
                          "\nPreis erklärt nur ",
                          round(get_regression_summaries(modell_einfach)$r_squared * 100, 1),
                          "% der Varianz!"),
           color = "#D50006", size = 3.2, hjust = 0,
           fontface = "bold", fill = "white", label.size = 0.3) +
  labs(title   = "Einfachregression: Sales ~ Price",
       subtitle = "Underfitting: Breite Streuung trotz signifikantem Preis-Effekt",
       x = "Preis (USD)", y = "Sales (Tsd. Einheiten)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(color = "#D50006", face = "bold"))

p2_fit


# -----------------------------------------------------------------------------
# 2.3 | Residualanalyse: Wo versagt das Modell?
# -----------------------------------------------------------------------------

# Residuen berechnen und anfügen
lab2_residuals <- get_regression_points(modell_einfach) |>
  bind_cols(Carseats |> select(ShelveLoc, Urban, US, Advertising))

# Residualplot: Muster = fehlende Variable!
p2_resid <- ggplot(lab2_residuals,
                   aes(x = Sales_hat, y = residual,
                       color = ShelveLoc)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_hline(yintercept = 0, color = "#686868",
             linewidth = 1, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE,
              linewidth = 0.8, aes(group = 1),
              color = "#333333") +
  scale_color_manual(values = c("Bad"    = "#D50006",
                                "Medium" = "#b3b2b2",
                                "Good"   = "#502479")) +
  labs(title   = "Residualplot — gefärbt nach ShelveLoc",
       subtitle = "Systematisches Muster = Omitted Variable Bias!",
       x = "Vorhergesagter Wert (Sales_hat)",
       y = "Residuum",
       color = "ShelveLoc") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(color = "#502479", face = "bold"))

p2_fit + p2_resid

# ┌──────────────────────────────────────────────────────────────────────────┐
# │ DEBRIEFING Lab 2 (Folie 14):                                             │
# │                                                                          │
# │ Q1: R² ist niedrig (~0.15). Was bedeutet das für die Aussagekraft?       │
# │                                                                          │
# │ Q2: Die Residuen sind nach ShelveLoc gefärbt und zeigen ein Muster.      │
# │     Was bedeutet das statistisch?                                        │
# │     → Omitted Variable Bias: ShelveLoc beeinflusst Sales UND            │
# │       ist mit Price korreliert → verzerrte Koeffizienten!                │
# │                                                                          │
# │ Q3: Was fehlt in unserem Modell für eine fundierte Underperformer-       │
# │     Entscheidung?                                                        │
# └──────────────────────────────────────────────────────────────────────────┘

# Zusammenhang ShelveLoc und Price prüfen (Confounder-Check)
Carseats |>
  group_by(ShelveLoc) |>
  summarise(
    avg_price  = mean(Price),
    avg_sales  = mean(Sales),
    avg_adv    = mean(Advertising),
    n          = n()
  )


