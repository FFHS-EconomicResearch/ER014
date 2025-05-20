library(tidyverse)
library(openintro)
tbl_mario <- mariokart
head(tbl_mario)


tbl_mario %>%
  filter(total_pr<100) %>%
  ggplot(aes(x=cond, y=total_pr)) +
    geom_jitter() + geom_smooth(method = "lm",se=FALSE)


# Levels explizit tauschen
tbl_mario <- tbl_mario %>%
  mutate(
    tmp = factor(recode(cond, "new" = "neu", "used" = "gebraucht")),  # tmp als Factor speichern
    zustand = factor(tmp, levels = rev(levels(tmp)))  # Reihenfolge umkehren
  )
# Regression mit neuer Kodierung
model <- lm(total_pr ~ as.numeric(zustand), data = tbl_mario %>% filter(total_pr < 100))
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Plot mit korrekt ausgerichteter Regressionsgerade
tbl_mario %>%
  filter(total_pr < 100) %>%
  ggplot(aes(x = zustand, y = total_pr)) +
  geom_jitter(width = 0.2,color="#4675BD") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = intercept, slope = slope, color = "#7A7770") + # Verlängerte Linie
  labs(x = "Zustand", y = "Gesamtpreis") + # Achsenbeschriftung
  theme_light()

