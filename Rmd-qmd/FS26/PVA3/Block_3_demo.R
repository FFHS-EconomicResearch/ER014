# ============================================================
# Block 3 – Live-Demo: Komplexität, Kausalität & Generalisierung
# Data Science and Strategy for Business – PVA 3
# ============================================================

# ---- Setup ----
library(tidyverse)
library(car)        # VIF
library(glmnet)     # Lasso/Ridge
library(caret)      # CV
library(lmtest)     # Diagnostik
library(pROC)       # AUC
library(effectsize) # Cohen's d

set.seed(2026)

# ---- 1. Daten generieren ----
n <- 500
daten <- tibble(
  werbeausgaben = rnorm(n, 50, 15),
  preis         = rnorm(n, 100, 20),
  qualitaet     = rnorm(n, 7, 2),
  marketing     = 0.8 * werbeausgaben + rnorm(n, 0, 5),  # kollinear!
  zufall        = rnorm(n, 0, 10),                        # kein Effekt
  saison        = sample(1:4, n, replace = TRUE)
) %>%
  mutate(
    saison_eff = c(-10, 5, 15, 20)[saison],
    umsatz = 200 + 1.5*werbeausgaben - 0.8*preis + 3*qualitaet + 
             saison_eff + rnorm(n, 0, 15),
    kauf   = rbinom(n, 1, plogis(-2 + 0.03*werbeausgaben - 0.01*preis + 0.2*qualitaet))
  )

# ---- 2. BLUE-Annahmen prüfen ----
cat("\n===== BLUE-Annahmen =====\n")
mod <- lm(umsatz ~ werbeausgaben + preis + qualitaet + saison_eff, data = daten)
summary(mod)
par(mfrow = c(2,2)); plot(mod); par(mfrow = c(1,1))
bptest(mod)   # Homoskedastizität
dwtest(mod)   # Autokorrelation

# ---- 3. Multikollinearität ----
cat("\n===== Multikollinearität =====\n")
mod_mc <- lm(umsatz ~ werbeausgaben + marketing + preis + qualitaet, data = daten)
vif(mod_mc)   # VIF > 5 → problematisch!

# Lösung: marketing entfernen
mod_clean <- lm(umsatz ~ werbeausgaben + preis + qualitaet, data = daten)
vif(mod_clean)

# ---- 4. Overfitting Demo ----
cat("\n===== Overfitting =====\n")
idx <- sample(1:n, 350)
train <- daten[idx, ]
test  <- daten[-idx, ]

m1 <- lm(umsatz ~ werbeausgaben, data = train)
m2 <- lm(umsatz ~ werbeausgaben + preis + qualitaet, data = train)
m3 <- lm(umsatz ~ werbeausgaben + preis + qualitaet + marketing + zufall + saison_eff,
         data = train)

cat("Train RMSE:", sqrt(mean(resid(m1)^2)), sqrt(mean(resid(m2)^2)), sqrt(mean(resid(m3)^2)), "\n")
cat("Test  RMSE:", 
    sqrt(mean((test$umsatz - predict(m1, test))^2)),
    sqrt(mean((test$umsatz - predict(m2, test))^2)),
    sqrt(mean((test$umsatz - predict(m3, test))^2)), "\n")

# ---- 5. Regularisierung (Lasso) ----
cat("\n===== Lasso-Regularisierung =====\n")
X <- model.matrix(umsatz ~ werbeausgaben + preis + qualitaet + marketing + zufall + saison_eff,
                  data = train)[, -1]
y <- train$umsatz

cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)
plot(cv_lasso, main = "Lasso: CV-MSE vs. Lambda")
cat("Bestes Lambda:", cv_lasso$lambda.min, "\n")
coef(cv_lasso, s = "lambda.min")   # Welche Variablen bleiben?

# Test-Performance
X_test <- model.matrix(umsatz ~ werbeausgaben + preis + qualitaet + marketing + zufall + saison_eff,
                       data = test)[, -1]
pred_lasso <- predict(cv_lasso, newx = X_test, s = "lambda.min")
cat("Lasso Test RMSE:", sqrt(mean((test$umsatz - pred_lasso)^2)), "\n")

# ---- 6. Kreuzvalidierung ----
cat("\n===== 10-Fold CV =====\n")
ctrl <- trainControl(method = "cv", number = 10)
cv_mod <- train(umsatz ~ werbeausgaben + preis + qualitaet, 
                data = daten, method = "lm", trControl = ctrl)
print(cv_mod)

# ---- 7. Scheinkorrelation ----
cat("\n===== Korrelation vs. Kausalität =====\n")
temp <- rnorm(100, 20, 8)
eis  <- 50 + 3*temp + rnorm(100, 0, 10)
ertr <- 2 + 0.5*temp + rnorm(100, 0, 3)
cat("Naive Korrelation (Eis vs. Ertrinken):", cor(eis, ertr), "\n")
cat("Partielle Korrelation:", cor(resid(lm(eis ~ temp)), resid(lm(ertr ~ temp))), "\n")

# ---- 8. A/B-Test ----
cat("\n===== A/B-Test =====\n")
a <- rnorm(500, 5.0, 3)
b <- rnorm(500, 5.8, 3)
t.test(a, b)
cat("Cohen's d:", cohens_d(c(a, b) ~ rep(c("A","B"), each = 500))$Cohens_d, "\n")

# ---- 9. Logistische Regression ----
cat("\n===== Logistische Regression =====\n")
logit <- glm(kauf ~ werbeausgaben + preis + qualitaet, data = train, family = binomial)
summary(logit)

# Odds Ratios
cat("Odds Ratios:\n")
print(round(exp(coef(logit)), 4))
print(round(exp(confint(logit)), 4))

# AUC
pred_p <- predict(logit, newdata = test, type = "response")
roc_obj <- roc(test$kauf, pred_p)
cat("AUC:", auc(roc_obj), "\n")
plot(roc_obj, main = paste("ROC-Kurve, AUC =", round(auc(roc_obj), 3)))

cat("\n===== Demo abgeschlossen! =====\n")
