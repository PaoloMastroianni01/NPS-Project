library(readxl)
library(DepthProc)
library(aplpack)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)
library(magrittr)
library(KernSmooth)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
library(MuMIn)
set.seed(19)

data_scientist <- read.csv("reduced_dataset.csv",header=T)
data_filtered <- data_scientist %>% filter(!is.na(ConvertedCompYearly)& !is.na(WorkExp))

summary(aov(ConvertedCompYearly~EdLevel+OrgSize ,data = data_filtered))
#WORKFLOW
#COLUMNS TO REMOVE A PRIORI --------------
data_filtered <- data_scientist %>% filter(!is.na(ConvertedCompYearly) & !is.na(WorkExp))

data_europe <- data_filtered %>%
  filter((Country != "United States of America" & Country != "Canada")& WorkExp <= 20 & EdLevel!="Some college/university study without earning a degree") %>%
  group_by(Country) %>%
  mutate(count = n()) %>%
  filter(count >= 7) %>%
  ungroup() %>%
  mutate(Region = ifelse(Country == "Italy", "Italy", "Other Europe")) 
  
data_europe <- data_europe[,-17]

data_america <- data_filtered %>%
  filter((Country == "United States of America" | Country == "Canada") & WorkExp <= 20& EdLevel!="Some college/university study without earning a degree") %>%
  mutate(Region = "America")

datafull <- rbind(data_america, data_europe)
depth_values <- depthContour(cbind(datafull$WorkExp, datafull$ConvertedCompYearly), 
                             depth_params = list(method = "Tukey"),ylim = c(0,750000))

bagplot_data <- bagplot(cbind(datafull$WorkExp, datafull$ConvertedCompYearly), 
                        factor = 3, show.whiskers = TRUE,ylim=c(0,750000))
outliers <- bagplot_data$pxy.outlier
outliers

# Combina WorkExp e ConvertedCompYearly in una matrice per confronto
coordinate <- cbind(datafull$WorkExp, datafull$ConvertedCompYearly)

# Identifica quali righe NON sono outlier
non_outliers_index <- !apply(coordinate, 1, function(row) any(row[1] == outliers[,1] & row[2] == outliers[,2]))

# Filtra i dati per rimuovere gli outlier
data_filtered_no_outliers <- datafull[non_outliers_index, ]
#ASSESS SIGNIFICANCY OF CATEGORICAL VARIABLES WITH ANOVA ------------

#SPLIT EUROPE AND AMERICA+CANADA!

#EdLevel
#OrgSize
#IcorPM (TEST DI IPOTESI)

attach(data_filtered_no_outliers)
OrgSize_new <- recode_factor(OrgSize,
                             "2 to 9 employees" = "2 to 99",
                             "10 to 19 employees" = "2 to 99",
                             "20 to 99 employees" = "2 to 99",
                             "100 to 499 employees" = "100 to 1000",
                             "500 to 999 employees" = "100 to 1000",
                             "1,000 to 4,999 employees" = "1000 to 10000",
                             "5,000 to 9,999 employees" = "1000 to 10000",
                             "10,000 or more employees" = "10000 or more"
)

# Ensure the new factor respects the intended order
OrgSizen <- factor(OrgSize_new, levels = c(
  "2 to 99", "100 to 1000", "1000 to 10000", "10000 or more"
))

plot(WorkExp,ConvertedCompYearly)
model_cubic_splines_2 <-
  lmer(ConvertedCompYearly ~ bs(WorkExp, degree = 3,df = 7)+(1|Country)+ EdLevel + OrgSizen,data= data_filtered_no_outliers)
summary(model_cubic_splines_2)
sigma2_eps <- as.numeric(get_variance_residual(model_cubic_splines_2))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(model_cubic_splines_2))
sigma2_b
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE
r.squaredGLMM(model_cubic_splines_2)
dotplot(ranef(model_cubic_splines_2, condVar=T))

detach(data_filtered_no_outliers)
attach(data_america)

model_cubic_splines_2 <-
  lm(ConvertedCompYearly ~ bs(WorkExp, degree = 3,df = 7)+EdLevel)
summary(model_cubic_splines_2)

EdLevels <- levels(as.factor(EdLevel)) # Get valid levels of OrgSize
WorkExp.grid <- seq(range(WorkExp)[1], range(WorkExp)[2], by = 1) # Generate WorkExp grid

# Create a data frame for predictions with each OrgSize level
prediction_grid <- expand.grid(WorkExp = WorkExp.grid, EdLevel = EdLevels)

# Generate predictions
preds=predict(model_cubic_splines_2, prediction_grid,se=T)
# Combine predictions with the grid for easier plotting
prediction_grid$fit <- preds$fit
prediction_grid$se_upper <- preds$fit + 2 * preds$se.fit
prediction_grid$se_lower <- preds$fit - 2 * preds$se.fit

# Base plot
plot(WorkExp, ConvertedCompYearly, xlim = range(WorkExp.grid), cex = 0.5, col = "darkgrey", main = "Cubic Splines")

# Add lines for each OrgSize level
colors <- c("red","black","darkblue","forestgreen") # Define colors for OrgSize levels
for (i in seq_along(EdLevels)) {
  subset_data <- prediction_grid[prediction_grid$EdLevel == EdLevels[i], ]
  
  # Add predicted fit line
  lines(subset_data$WorkExp, subset_data$fit, col = colors[i], lwd = 2)
  
  # Add confidence intervals
  matlines(subset_data$WorkExp, cbind(subset_data$se_upper, subset_data$se_lower), col = colors[i], lwd = 1, lty = 3)
}
# Add legend
legend("topright", legend = EdLevels, col = colors, lwd = 1, cex=0.7, title = "EdLevel")


knots <- attr(bs(WorkExp, degree=3,df=7),'knots')
abline(v = knots, lty=3)

OrgSize_new <- recode_factor(OrgSize,
                             "2 to 9 employees" = "2 to 99",
                             "10 to 19 employees" = "2 to 99",
                             "20 to 99 employees" = "2 to 99",
                             "100 to 499 employees" = "100 to 1000",
                             "500 to 999 employees" = "100 to 1000",
                             "1,000 to 4,999 employees" = "1000 to 10000",
                             "5,000 to 9,999 employees" = "1000 to 10000",
                             "10,000 or more employees" = "10000 or more"
)

# Ensure the new factor respects the intended order
OrgSizen <- factor(OrgSize_new, levels = c(
  "2 to 99", "100 to 1000", "1000 to 10000", "10000 or more"
))


model_cubic_splines_2 <-
  lm(ConvertedCompYearly ~ bs(WorkExp, degree = 3,df = 7)+OrgSizen)
summary(model_cubic_splines_2)

OrgSizes <- levels(as.factor(OrgSizen)) # Get valid levels of OrgSize
WorkExp.grid <- seq(range(WorkExp)[1], range(WorkExp)[2], by = 1) # Generate WorkExp grid

# Create a data frame for predictions with each OrgSize level
prediction_grid <- expand.grid(WorkExp = WorkExp.grid, OrgSizen = OrgSizes)

# Generate predictions
preds=predict(model_cubic_splines_2, prediction_grid,se=T)
# Combine predictions with the grid for easier plotting
prediction_grid$fit <- preds$fit
prediction_grid$se_upper <- preds$fit + 2 * preds$se.fit
prediction_grid$se_lower <- preds$fit - 2 * preds$se.fit

# Base plot
plot(WorkExp, ConvertedCompYearly, xlim = range(WorkExp.grid), cex = 0.5, col = "darkgrey", main = "Cubic Splines")

# Add lines for each OrgSize level
colors <- c("red","black","darkblue","forestgreen") # Define colors for OrgSize levels
for (i in seq_along(OrgSizes)) {
  subset_data <- prediction_grid[prediction_grid$OrgSize == OrgSizes[i], ]
  
  # Add predicted fit line
  lines(subset_data$WorkExp, subset_data$fit, col = colors[i], lwd = 2)
  
  # Add confidence intervals
  matlines(subset_data$WorkExp, cbind(subset_data$se_upper, subset_data$se_lower), col = colors[i], lwd = 1, lty = 3)
}
# Add legend
legend("topright", legend = OrgSizes, col = colors, lwd = 1, cex=0.7, title = "OrgSize")


knots <- attr(bs(WorkExp, degree=3,df=7),'knots')
abline(v = knots, lty=3)
detach(data_america)
attach(data_europe)


model_cubic_splines_2 <-
  lm(ConvertedCompYearly ~ bs(WorkExp, degree = 3,df = 7)+EdLevel)
summary(model_cubic_splines_2)

EdLevels <- levels(as.factor(EdLevel)) # Get valid levels of OrgSize
WorkExp.grid <- seq(range(WorkExp)[1], range(WorkExp)[2], by = 1) # Generate WorkExp grid

# Create a data frame for predictions with each OrgSize level
prediction_grid <- expand.grid(WorkExp = WorkExp.grid, EdLevel = EdLevels)

# Generate predictions
preds=predict(model_cubic_splines_2, prediction_grid,se=T)
# Combine predictions with the grid for easier plotting
prediction_grid$fit <- preds$fit
prediction_grid$se_upper <- preds$fit + 2 * preds$se.fit
prediction_grid$se_lower <- preds$fit - 2 * preds$se.fit

# Base plot
plot(WorkExp, ConvertedCompYearly, xlim = range(WorkExp.grid), cex = 0.5, col = "darkgrey", main = "Cubic Splines")

# Add lines for each OrgSize level
colors <- c("red","black","darkblue","forestgreen") # Define colors for OrgSize levels
for (i in seq_along(EdLevels)) {
  subset_data <- prediction_grid[prediction_grid$EdLevel == EdLevels[i], ]
  
  # Add predicted fit line
  lines(subset_data$WorkExp, subset_data$fit, col = colors[i], lwd = 2)
  
  # Add confidence intervals
  matlines(subset_data$WorkExp, cbind(subset_data$se_upper, subset_data$se_lower), col = colors[i], lwd = 1, lty = 3)
}
# Add legend
legend("topright", legend = EdLevels, col = colors, lwd = 1, cex=0.7, title = "EdLevel")


knots <- attr(bs(WorkExp, degree=3,df=7),'knots')
abline(v = knots, lty=3)

OrgSize_new <- recode_factor(OrgSize,
                             "2 to 9 employees" = "2 to 99",
                             "10 to 19 employees" = "2 to 99",
                             "20 to 99 employees" = "2 to 99",
                             "100 to 499 employees" = "100 to 1000",
                             "500 to 999 employees" = "100 to 1000",
                             "1,000 to 4,999 employees" = "1000 to 10000",
                             "5,000 to 9,999 employees" = "1000 to 10000",
                             "10,000 or more employees" = "10000 or more"
)

# Ensure the new factor respects the intended order
OrgSizen <- factor(OrgSize_new, levels = c(
  "2 to 99", "100 to 1000", "1000 to 10000", "10000 or more"
))


model_cubic_splines_2 <-
  lm(ConvertedCompYearly ~ bs(WorkExp, degree = 3,df = 7)+OrgSizen)
summary(model_cubic_splines_2)

OrgSizes <- levels(as.factor(OrgSizen)) # Get valid levels of OrgSize
WorkExp.grid <- seq(range(WorkExp)[1], range(WorkExp)[2], by = 1) # Generate WorkExp grid

# Create a data frame for predictions with each OrgSize level
prediction_grid <- expand.grid(WorkExp = WorkExp.grid, OrgSizen = OrgSizes)

# Generate predictions
preds=predict(model_cubic_splines_2, prediction_grid,se=T)
# Combine predictions with the grid for easier plotting
prediction_grid$fit <- preds$fit
prediction_grid$se_upper <- preds$fit + 2 * preds$se.fit
prediction_grid$se_lower <- preds$fit - 2 * preds$se.fit

# Base plot
plot(WorkExp, ConvertedCompYearly, xlim = range(WorkExp.grid), cex = 0.5, col = "darkgrey", main = "Cubic Splines")

# Add lines for each OrgSize level
colors <- c("red","black","darkblue","forestgreen") # Define colors for OrgSize levels
for (i in seq_along(OrgSizes)) {
  subset_data <- prediction_grid[prediction_grid$OrgSize == OrgSizes[i], ]
  
  # Add predicted fit line
  lines(subset_data$WorkExp, subset_data$fit, col = colors[i], lwd = 2)
  
  # Add confidence intervals
  matlines(subset_data$WorkExp, cbind(subset_data$se_upper, subset_data$se_lower), col = colors[i], lwd = 1, lty = 3)
}
# Add legend
legend("topright", legend = OrgSizes, col = colors, lwd = 1, cex=0.7, title = "OrgSize")


knots <- attr(bs(WorkExp, degree=3,df=7),'knots')
abline(v = knots, lty=3)
detach(data_europe)

attach(data_filtered_no_outliers)
model_cubic_splines_2 <-
  lm(ConvertedCompYearly ~ bs(WorkExp, degree = 3,df = 7)+Region)
summary(model_cubic_splines_2)

Regions <- levels(as.factor(Region)) # Get valid levels of OrgSize
WorkExp.grid <- seq(range(WorkExp)[1], range(WorkExp)[2], by = 1) # Generate WorkExp grid

# Create a data frame for predictions with each OrgSize level
prediction_grid <- expand.grid(WorkExp = WorkExp.grid, Region = Regions)

# Generate predictions
preds=predict(model_cubic_splines_2, prediction_grid,se=T)
# Combine predictions with the grid for easier plotting
prediction_grid$fit <- preds$fit
prediction_grid$se_upper <- preds$fit + 2 * preds$se.fit
prediction_grid$se_lower <- preds$fit - 2 * preds$se.fit

# Base plot
plot(WorkExp, ConvertedCompYearly, xlim = range(WorkExp.grid), cex = 0.5, col = "darkgrey", main = "Cubic Splines")

# Add lines for each OrgSize level
colors <- c("red","darkblue","forestgreen") # Define colors for OrgSize levels
for (i in seq_along(Regions)) {
  subset_data <- prediction_grid[prediction_grid$Region == Regions[i], ]
  
  # Add predicted fit line
  lines(subset_data$WorkExp, subset_data$fit, col = colors[i], lwd = 2)
  
  # Add confidence intervals
  matlines(subset_data$WorkExp, cbind(subset_data$se_upper, subset_data$se_lower), col = colors[i], lwd = 1, lty = 3)
}
# Add legend
legend("topright", legend = Regions, col = colors, lwd = 1, cex=0.7, title = "Region")


knots <- attr(bs(WorkExp, degree=3,df=7),'knots')
abline(v = knots, lty=3)
#REGRESSION WITH SIGNIFICANT VARIABLES USING LAB7+LAB8 (GAM) METHODS ---------------

#Remove outliers? (using bagplot: income-work experience)

#Country (random intercept?)
#Work experience
#Significant Categorical Variables 

#find combination for GAM (look lab.8)

#Comparison of Europe and America regression lines (no intercept)


#QUANTILE REGRESSION -------------