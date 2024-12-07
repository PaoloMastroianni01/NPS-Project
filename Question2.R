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

#LOAD AND CLEAN DATASETS -----------
data_scientist <- read.csv("reduced_dataset.csv",header=T)
data_filtered <- data_scientist %>% filter(!is.na(ConvertedCompYearly)& !is.na(WorkExp))
data_filtered <- data_filtered[,c(3:8)]

#obtain only europe countries with at least 7 observations and with maximum 20 years of experience (due to lack of data)
data_europe <- data_filtered %>%
  filter((Country != "United States of America" & Country != "Canada")& WorkExp <= 20 & EdLevel!="Some college/university study without earning a degree") %>%
  mutate(Region = ifelse(Country == "Italy", "Italy", "Other Europe")) 
data_europe <- data_europe[,-c(3,4)]

#find outliers and remove
depth_values <- depthContour(cbind(data_europe$WorkExp, data_europe$ConvertedCompYearly), 
                             depth_params = list(method = "Tukey"),ylim = c(0,750000))

bagplot_data <- bagplot(cbind(data_europe$WorkExp, data_europe$ConvertedCompYearly), 
                        factor = 3, show.whiskers = TRUE,ylim=c(0,750000))
outliers <- bagplot_data$pxy.outlier
coordinate <- cbind(data_europe$WorkExp, data_europe$ConvertedCompYearly)
non_outliers_index <- !apply(coordinate, 1, function(row) any(row[1] == outliers[,1] & row[2] == outliers[,2]))
data_europe <- data_europe[non_outliers_index, ]

#the same for america
data_america <- data_filtered %>%
  filter((Country == "United States of America" | Country == "Canada") & WorkExp <= 20 & EdLevel!="Some college/university study without earning a degree") %>%
  mutate(Region = "America")
data_america <- data_america[,-c(3,4)]

depth_values <- depthContour(cbind(data_america$WorkExp, data_america$ConvertedCompYearly), 
                             depth_params = list(method = "Tukey"),ylim = c(0,750000))

bagplot_data <- bagplot(cbind(data_america$WorkExp, data_america$ConvertedCompYearly), 
                        factor = 3, show.whiskers = TRUE,ylim=c(0,750000))
outliers <- bagplot_data$pxy.outlier
coordinate <- cbind(data_america$WorkExp, data_america$ConvertedCompYearly)
non_outliers_index <- !apply(coordinate, 1, function(row) any(row[1] == outliers[,1] & row[2] == outliers[,2]))
data_america <- data_america[non_outliers_index, ]

#join all
data_full <- rbind(data_america, data_europe)

depth_values <- depthContour(cbind(data_full$WorkExp, data_full$ConvertedCompYearly), 
                             depth_params = list(method = "Tukey"),ylim = c(0,750000))

bagplot_data <- bagplot(cbind(data_full$WorkExp, data_full$ConvertedCompYearly), 
                        factor = 3, show.whiskers = TRUE,ylim=c(0,750000))
outliers <- bagplot_data$pxy.outlier
coordinate <- cbind(data_full$WorkExp, data_full$ConvertedCompYearly)
non_outliers_index <- !apply(coordinate, 1, function(row) any(row[1] == outliers[,1] & row[2] == outliers[,2]))
data_full <- data_full[non_outliers_index, ]

#redefine the organization size factor
datasets <- list(data_europe = data_europe, data_america = data_america, data_full = data_full)
#ASSESS SIGNIFICANCY OF CATEGORICAL VARIABLES WITH ANOVA ------------
B <- 10000
anova_results <- data.frame(
  Dataset = character(),
  Categorical_Variable = character(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

perform_permutation_anova <- function(data, response, categorical, B = 10000) {
  response_values <- data[[response]]
  categorical_values <- data[[categorical]]
  
  fit <- aov(response_values ~ categorical_values)
  T0 <- summary(fit)[[1]][1, 4]
  
  T_stat <- numeric(B)
  n <- nrow(data)
  for (perm in 1:B) {
    permuted_response <- sample(response_values)
    fit_perm <- aov(permuted_response ~ categorical_values)
    T_stat[perm] <- summary(fit_perm)[[1]][1, 4]
  }
  
  p_val <- sum(T_stat >= T0) / B
  return(p_val)
}

for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]
  
  #ANOVA for EdLevel
  p_val_edlevel <- perform_permutation_anova(data, response = "ConvertedCompYearly", categorical = "EdLevel", B = B)
  anova_results <- rbind(anova_results, data.frame(Dataset = dataset_name, Categorical_Variable = "EdLevel", P_Value = p_val_edlevel))
  
  #ANOVA for OrgSize
  p_val_orgsize <- perform_permutation_anova(data, response = "ConvertedCompYearly", categorical = "OrgSize", B = B)
  anova_results <- rbind(anova_results, data.frame(Dataset = dataset_name, Categorical_Variable = "OrgSize", P_Value = p_val_orgsize))
}

print(anova_results)

#REGRESSION AND PLOT FOR ORGANIZATION SIZE AND EDUCATIONAL LEVEL ---------------
datasets <- list(america = data_america, europe = data_europe)

# create model and plot for educational level
process_and_plot_ed_level <- function(df, dataset_name) {
  #cubic spline with 4 knots
  model_cubic_splines_2 <- lm(ConvertedCompYearly ~ bs(WorkExp, degree = 3, df = 7) + EdLevel, data = df)
  EdLevels <- levels(as.factor(df$EdLevel))
  
  WorkExp.grid <- seq(range(df$WorkExp, na.rm = TRUE)[1], range(df$WorkExp, na.rm = TRUE)[2], by = 1)
  prediction_grid <- expand.grid(WorkExp = WorkExp.grid, EdLevel = EdLevels)
  
  preds <- predict(model_cubic_splines_2, newdata = prediction_grid, se = TRUE)
  prediction_grid$fit <- preds$fit
  prediction_grid$se_upper <- preds$fit + 2 * preds$se.fit
  prediction_grid$se_lower <- preds$fit - 2 * preds$se.fit
  
  p <- ggplot() +
    geom_point(data = df, aes(x = WorkExp, y = ConvertedCompYearly), alpha = 0.5, color = "darkgrey") +
    geom_line(data = prediction_grid, aes(x = WorkExp, y = fit, color = EdLevel), size = 1) +
    #geom_ribbon(data = prediction_grid, aes(x = WorkExp, ymin = se_lower, ymax = se_upper, fill = EdLevel), alpha = 0.2) +
    scale_color_manual(values = c("red", "black", "darkblue", "forestgreen")) +
    scale_fill_manual(values = c("red", "black", "darkblue", "forestgreen")) +
    labs(
      title = paste("Cubic Splines -", dataset_name),
      x = "Work Experience",
      y = "Yearly Compensation",
      color = "Education Level",
      fill = "Education Level"
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    geom_vline(xintercept = attr(bs(df$WorkExp, degree = 3, df = 7), "knots"), linetype = "dashed", color = "grey")
  
  # Salva il plot o mostralo
  print(p)
}
process_and_plot_org_size <- function(df,dataset_name) {
  
  model_cubic_splines_2 <- lm(ConvertedCompYearly ~ bs(WorkExp, degree = 3, df = 7) + OrgSize, data = df)
  OrgSizes <- levels(as.factor(df$OrgSize))
  
  WorkExp.grid <- seq(range(df$WorkExp, na.rm = TRUE)[1], range(df$WorkExp, na.rm = TRUE)[2], by = 1)
  prediction_grid <- expand.grid(WorkExp = WorkExp.grid, OrgSize = OrgSizes)
  
  preds <- predict(model_cubic_splines_2, newdata = prediction_grid, se = TRUE)
  prediction_grid$fit <- preds$fit
  prediction_grid$se_upper <- preds$fit + 2 * preds$se.fit
  prediction_grid$se_lower <- preds$fit - 2 * preds$se.fit
  
  knots <- attr(bs(df$WorkExp, degree = 3, df = 7), "knots")
  
  p <- ggplot() +
    geom_point(data = df, aes(x = WorkExp, y = ConvertedCompYearly), alpha = 0.5, color = "darkgrey") +
    geom_line(data = prediction_grid, aes(x = WorkExp, y = fit, color = OrgSize), size = 1) +
    #geom_ribbon(data = prediction_grid, aes(x = WorkExp, ymin = se_lower, ymax = se_upper, fill = OrgSize), alpha = 0.2) +
    scale_color_manual(values = c("red", "black", "darkblue", "forestgreen")) +
    scale_fill_manual(values = c("red", "black", "darkblue", "forestgreen")) +
    labs(
      title = paste("Cubic Splines -", dataset_name),
      x = "Work Experience",
      y = "Yearly Compensation",
      color = "OrgSize",
      fill = "OrgSize"
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    geom_vline(xintercept = knots, linetype = "dashed", color = "grey")
  
  print(p)
}

for (name in names(datasets)) {
  process_and_plot_ed_level(datasets[[name]],name)
  process_and_plot_org_size(datasets[[name]],name)
}
#REGRESSION AND PLOT FOR REGION ----------
attach(data_full)
model_cubic_splines_2 <- lm(ConvertedCompYearly ~ bs(WorkExp, degree = 3,df = 7)+Region)
summary(model_cubic_splines_2)

Regions <- levels(as.factor(Region))
WorkExp.grid <- seq(range(WorkExp)[1], range(WorkExp)[2], by = 1) 


prediction_grid <- expand.grid(WorkExp = WorkExp.grid, Region = Regions)
preds=predict(model_cubic_splines_2, prediction_grid,se=T)

prediction_grid$fit <- preds$fit
prediction_grid$se_upper <- preds$fit + 2 * preds$se.fit
prediction_grid$se_lower <- preds$fit - 2 * preds$se.fit

# Nodi delle spline
knots <- attr(bs(WorkExp, degree = 3, df = 7), "knots")

# Visualizzazione con ggplot2
p <- ggplot() +
  geom_point(data = data_full, aes(x = WorkExp, y = ConvertedCompYearly), alpha = 0.5, color = "darkgrey") +
  geom_line(data = prediction_grid, aes(x = WorkExp, y = fit, color = Region), size = 1) +
  geom_ribbon(data = prediction_grid, aes(x = WorkExp, ymin = se_lower, ymax = se_upper, fill = Region), alpha = 0.2) +
  geom_vline(xintercept = knots, linetype = "dashed", color = "grey") +
  scale_color_manual(values = c("red", "darkblue", "forestgreen")) +
  scale_fill_manual(values = c("red", "darkblue", "forestgreen")) +
  labs(
    title = "Cubic Splines with Regions",
    x = "Work Experience",
    y = "Yearly Compensation",
    color = "Region",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p)
detach(data_full)

#FULL MODEL WITH RANDOM INTERCEPT ---------------
attach(data_full)

model_cubic_splines_2 <- lmer(ConvertedCompYearly ~ bs(WorkExp, degree = 3,df = 7)+(1|Country)+ EdLevel + OrgSize)
summary(model_cubic_splines_2)
sigma2_eps <- as.numeric(get_variance_residual(model_cubic_splines_2))
sigma2_b <- as.numeric(get_variance_random(model_cubic_splines_2))
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE
r.squaredGLMM(model_cubic_splines_2)
dotplot(ranef(model_cubic_splines_2, condVar=T))

detach(data_full)