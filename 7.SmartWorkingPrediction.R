library(splines)
library(conformalInference)

load('datasets_no_outliers.Rdata')
data_europe <- datasets[["data_europe"]]
data_america <- datasets[["data_america"]]
data_full <- datasets[["data_full"]]

attach(data_full)

#table(OrgSize, EdLevel)

# Matrix of features of the observed data (solo WorkExp, senza i fattori)
X_spline <- bs(WorkExp, degree = 2, df = 4)  # Spline su WorkExp
X <- X_spline  # Solo spline su WorkExp

# Vector of responses of the observed dataset
y <- ConvertedCompYearly

# Define grid for WorkExp and evaluate the spline basis
WorkExp.grid <- seq(min(WorkExp), max(WorkExp), by = 1)
X_test_spline <- bs(WorkExp.grid, degree = 2, df = 4)  # Spline per la griglia di WorkExp

# Funzioni di training e predizione per il Conformal Inference
lm_train <- lm.funs(intercept = TRUE)$train.fun
lm_predict <- lm.funs(intercept = TRUE)$predict.fun

# Perform conformal prediction
c_preds <- conformal.pred(x = X, y = y, x0 = X_test_spline,
                          alpha = 0.1, verbose = FALSE, train.fun = lm_train,
                          predict.fun = lm_predict, num.grid.pts = 200)

# Estrai le predizioni e gli intervalli di confidenza
predictions <- c_preds$pred
upper_bounds <- c_preds$up
lower_bounds <- c_preds$lo

# Visualizza i risultati
plot(WorkExp, ConvertedCompYearly, xlim = range(WorkExp.grid), cex = 0.5, col = "lightblue",
     main = 'Spline Regression with Conformal Prediction',
     xlab = "Work Experience (Years)", ylab = "Converted Compensation Yearly")
lines(WorkExp.grid, predictions, lwd = 2, col = "black", lty = 1)
matlines(WorkExp.grid, cbind(upper_bounds, lower_bounds), lwd = 1, col = "black", lty = 2)


detach(data_full)
