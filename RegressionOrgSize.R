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
set.seed(19)

data_scientist <- read.csv("reduced_dataset.csv",header=T)

#depth measures + bagplot
data_filtered <- data_scientist %>% filter(!is.na(ConvertedCompYearly) & !is.na(WorkExp))

data_europe <- data_filtered %>%
  filter((Country != "United States of America" & Country != "Canada")) %>%
  group_by(Country) %>%
  mutate(count = n()) %>%
  filter(count >= 10) %>%
  ungroup() %>%
  mutate(Region = ifelse(Country == "Italy", "Italy", "Other Europe")) %>%
  select(-count)


data_america <- data_filtered %>%
  filter((Country == "United States of America" | Country == "Canada")) %>%
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

dataAM <- data_filtered_no_outliers[data_filtered_no_outliers$Country == 'Canada' | data_filtered_no_outliers$Country == 'United States of America',c(4,8,9)]
dataEU <- data_filtered_no_outliers[data_filtered_no_outliers$Country != 'Canada' & data_filtered_no_outliers$Country != 'United States of America',c(4,8,9)]
datatutto <- data_filtered_no_outliers[,c(8,9,17)]

attach(dataAM)
# Define new categories
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

data1 = dataAM[which(OrgSizen == "2 to 99"),]
data2 = dataAM[which(OrgSizen == "100 to 1000"),]
data3 = dataAM[which(OrgSizen == "1000 to 10000"),]
data4 = dataAM[which(OrgSizen == "10000 or more"),]

plot(WorkExp,ConvertedCompYearly)

model_cubic_splines_1 <- lm(ConvertedCompYearly ~ bs(WorkExp, degree = 2, df = 4), data = data1)
model_cubic_splines_2 <- lm(ConvertedCompYearly ~ bs(WorkExp, degree = 2, df = 4), data = data2)
model_cubic_splines_3 <- lm(ConvertedCompYearly ~ bs(WorkExp, degree = 2, df = 4), data = data3)
model_cubic_splines_4 <- lm(ConvertedCompYearly ~ bs(WorkExp, degree = 2, df = 4), data = data4)

OrgSizes <- levels(OrgSizen)

WorkExp.grid <- seq(1,15, by = 1)
prediction_grid <- expand.grid(WorkExp = WorkExp.grid, OrgSize = OrgSizes)

# Generate predictions for model 1
preds1 <- predict(model_cubic_splines_1, newdata = prediction_grid, se = TRUE)
prediction_grid$fit1 <- preds1$fit
prediction_grid$se_upper1 <- preds1$fit + 2 * preds1$se.fit
prediction_grid$se_lower1 <- preds1$fit - 2 * preds1$se.fit

# Generate predictions for model 2
preds2 <- predict(model_cubic_splines_2, newdata = prediction_grid, se = TRUE)
prediction_grid$fit2 <- preds2$fit
prediction_grid$se_upper2 <- preds2$fit + 2 * preds2$se.fit
prediction_grid$se_lower2 <- preds2$fit - 2 * preds2$se.fit

# Generate predictions for model 3
preds3 <- predict(model_cubic_splines_3, newdata = prediction_grid, se = TRUE)
prediction_grid$fit3 <- preds3$fit
prediction_grid$se_upper3 <- preds3$fit + 2 * preds3$se.fit
prediction_grid$se_lower3 <- preds3$fit - 2 * preds3$se.fit

# Generate predictions for model 4
preds4 <- predict(model_cubic_splines_4, newdata = prediction_grid, se = TRUE)
prediction_grid$fit4 <- preds4$fit
prediction_grid$se_upper4 <- preds4$fit + 2 * preds4$se.fit
prediction_grid$se_lower4 <- preds4$fit - 2 * preds4$se.fit

library(tidyr)

# Reshape the data manually
plot_data <- reshape(
  prediction_grid,
  varying = list(fit = c("fit1", "fit2", "fit3", "fit4"),
                 se_upper = c("se_upper1", "se_upper2", "se_upper3", "se_upper4"),
                 se_lower = c("se_lower1", "se_lower2", "se_lower3", "se_lower4")),
  v.names = c("fit", "se_upper", "se_lower"),
  timevar = "model",
  times = c("model1", "model2", "model3", "model4"),
  direction = "long"
)

ggplot(plot_data, aes(x = WorkExp, y = fit, color = model, group = model)) +
  geom_line() +
  #geom_ribbon(aes(ymin = se_lower, ymax = se_upper, fill = model), alpha = 0.2) +
  labs(title = "Cubic Spline Predictions with Confidence Intervals",
       x = "Work Experience (Years)",
       y = "Predicted Yearly Compensation",
       color = "Model",
       fill = "Model") +
  theme_minimal()


detach(dataAM)
