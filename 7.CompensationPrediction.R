library(DepthProc)
library(aplpack)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(ISLR2)
library(splines)
library(KernSmooth)
library(nlmeU)
library(corrplot)
library(nlme)
library(lme4)
library(insight)
library(MuMIn)
library(lattice)
library(splines)
library(conformalInference)
set.seed(19)

#LOAD AND CLEAN DATASETS -----------
data_scientist <- read.csv("reduced_dataset.csv",header=T)
data_filtered <- data_scientist %>% filter(!is.na(ConvertedCompYearly)& !is.na(WorkExp))
data_filtered <- data_filtered[,c(3:8)]

data_america <- data_filtered %>%
  filter((Country == "United States of America") & WorkExp <= 15 & EdLevel!="Some college/university study without earning a degree") %>%
  mutate(Region = "America")
data_america <- data_america[,-4]

depth_values <- depthContour(cbind(data_america$WorkExp, data_america$ConvertedCompYearly), 
                             depth_params = list(method = "Tukey"),ylim = c(0,750000))

bagplot_data <- bagplot(cbind(data_america$WorkExp, data_america$ConvertedCompYearly), 
                        factor = 3, show.whiskers = TRUE,ylim=c(0,750000))
outliers <- bagplot_data$pxy.outlier
coordinate <- cbind(data_america$WorkExp, data_america$ConvertedCompYearly)
non_outliers_index <- !apply(coordinate, 1, function(row) any(row[1] == outliers[,1] & row[2] == outliers[,2]))
data_america <- data_america[non_outliers_index, ]

attach(data_america)

# Matrix of features of the observed data
X_spline <- bs(WorkExp, degree=3, df=7)
X_factors <- model.matrix(~ EdLevel + OrgSize - 1)
X <- cbind(X_spline, X_factors)
# Vector of responses of the observed dataset
y <- ConvertedCompYearly

WorkExp.grid <- seq(range(WorkExp)[1], range(WorkExp)[2], by = 1)
# And evaluate the predictors over a grid
X_test_spline <- matrix(bs(WorkExp.grid, degree=3, df=7), nrow=length(WorkExp.grid))
# Ottieni i livelli unici delle variabili categoriche
levels_EdLevel <- unique(EdLevel)
levels_OrgSize <- unique(OrgSize)

# Crea tutte le combinazioni possibili
combinations <- expand.grid(EdLevel = levels_EdLevel, OrgSize = levels_OrgSize)

# Genera la model.matrix basata sulle combinazioni
X_test_factors <- model.matrix(~ EdLevel + OrgSize - 1, data = combinations) 
X_test_factors <- matrix(rep(t(X_test_factors), each = nrow(X_test_spline)), 
                         ncol = ncol(X_test_factors), 
                         byrow = FALSE)
X_test_spline <- matrix(rep(as.vector(X_test_spline), 12), 
                                 ncol = ncol(X_test_spline), 
                                 byrow = FALSE)
cat("Righe X_test_spline:", nrow(X_test_spline), "\n")
cat("Righe X_test_factors:", nrow(X_test_factors), "\n")
X_test_grid <- cbind(X_test_spline, X_test_factors)
# Function to perform model training
lm_train = lm.funs(intercept = T)$train.fun

# Function to perform point prediction
lm_predict = lm.funs(intercept = T)$predict.fun

c_preds <- conformal.pred(x=X, y=y, x0=X_test_grid,
                          alpha=0.1, verbose=F, train.fun = lm_train,
                          predict.fun=lm_predict, num.grid.pts=200)

plot(WorkExp, ConvertedCompYearly, xlim=range(WorkExp.grid), cex=.5, col ="lightblue",
     main='Spline Regression')
lines(WorkExp.grid, c_preds$pred, lwd=2, col="black", lty=1)
matlines(WorkExp.grid, cbind(c_preds$up,c_preds$lo), lwd=1, col="black", lty=2)