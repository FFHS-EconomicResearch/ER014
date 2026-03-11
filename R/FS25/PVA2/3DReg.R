#knitr::include_graphics('https://i.stack.imgur.com/3pmXi.png')



library(ggplot2)
library(plotly)

# Beispiel Daten generieren
set.seed(123)
data <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  y = 2 * data$x1 - 3 * data$x2 + rnorm(100)
)

# OLS-Modell anpassen
model <- lm(y ~ x1 + x2, data = data)

# Vorhersagen des Modells
data$pred <- predict(model)

# 3D-Diagramm mit plot_ly erstellen
p <- plot_ly(data, x = ~x1, y = ~x2, z = ~y, type = "scatter3d", mode = "markers", marker = list(size = 5)) %>%
  add_surface(
    x = data$x1,
    y = data$x2,
    z = matrix(predict(model), nrow = length(unique(data$x1)), byrow = TRUE),
    opacity = 0.6
  ) %>%
  layout(scene = list(xaxis = list(title = "x1"), yaxis = list(title = "x2"), zaxis = list(title = "y")))

# Anzeigen des interaktiven 3D-Diagramms
p



# Hinzufügen Regressionsebene -----






library(ggplot2)
library(plotly)

# Beispiel Daten generieren
set.seed(123)
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- 2 * x1 - 3 * x2 + rnorm(100)
data <- data.frame(x1 = x1, x2 = x2, y = y)
# OLS-Modell anpassen
model <- lm(y ~ x1 + x2, data = data)

# Extrahieren der Koeffizienten des Modells
intercept <- coef(model)[1]
coef_x1 <- coef(model)[2]
coef_x2 <- coef(model)[3]

# Berechnen der Werte für die Regressionsfläche basierend auf den Modellkoeffizienten
x_grid <- seq(min(data$x1), max(data$x1), length.out = 30)
y_grid <- seq(min(data$x2), max(data$x2), length.out = 30)
z <- outer(x_grid, y_grid, function(x1, x2) intercept + coef_x1 * x1 + coef_x2 * x2)

# 3D-Diagramm mit Beobachtungen und Regressionsfläche erstellen
p <- plot_ly() %>%
  add_trace(data = data, x = ~x1, y = ~x2, z = ~y, type = "scatter3d", mode = "markers", marker = list(size = 5)) %>%
  add_surface(x = x_grid, y = y_grid, z = z, colorscale = "Blues", opacity = 0.6) %>%
  layout(scene = list(xaxis = list(title = "x1"), yaxis = list(title = "x2"), zaxis = list(title = "y")))

# Anzeigen des interaktiven 3D-Diagramms mit Beobachtungen und Regressionsfläche
p




























