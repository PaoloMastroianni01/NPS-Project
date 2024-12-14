library(ggplot2)
library(tidyr)
library(dplyr)
library(conformalInference)
library(gridExtra)
library(grid) 

#load data and keep only year and if observation is full remote, full in-person or hybrid
df <- read.csv('reduced_dataset.csv')
df_sw <- df[,c(2,16)]

#change name for visual purpose
df_sw <- df_sw %>%
  mutate(RemoteWork = ifelse(RemoteWork == "Hybrid (some remote, some in-person)", "Hybrid", RemoteWork))

#compute percentages foe each level in each year
df_percent <- df_sw %>%
  group_by(Year, RemoteWork) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Year) %>%
  mutate(Percentage = 100 * Count / sum(Count))

#get dataset for each level over years
df_remote <- df_percent %>% filter(RemoteWork == "Remote")
df_hybrid <- df_percent %>% filter(RemoteWork == "Hybrid")
df_inperson <- df_percent %>% filter(RemoteWork == "In-person")

#function for regression and conformal prediction interval (90% interval)
perform_analysis <- function(data, level_name) {
  
  lm_model <- lm(Percentage ~ Year, data = data)
  
  #residuals for conformal prediction
  residuals <- abs(residuals(lm_model))
  
  #prediction for year 2025
  new_data <- data.frame(Year = 2025)
  pred <- predict(lm_model, newdata = new_data)
  
  #conformal prediction interval
  alpha <- 0.1
  q <- quantile(residuals, 1 - alpha)
  conf_interval <- c(pred - q, pred + q)
  
  list(model = lm_model, prediction = pred, conf_interval = conf_interval)
}

remote_analysis <- perform_analysis(df_remote, "Remote")
hybrid_analysis <- perform_analysis(df_hybrid, "Hybrid")
inperson_analysis <- perform_analysis(df_inperson, "In-person")

#table for 2025 based on predictions
conf_table <- data.frame(
  Level = c("Remote", "Hybrid", "In-person"),
  Prediction2025 = c(remote_analysis$prediction, 
                     hybrid_analysis$prediction, 
                     inperson_analysis$prediction),
  LowerBound = c(remote_analysis$conf_interval[1], 
                 hybrid_analysis$conf_interval[1], 
                 inperson_analysis$conf_interval[1]),
  UpperBound = c(remote_analysis$conf_interval[2], 
                 hybrid_analysis$conf_interval[2], 
                 inperson_analysis$conf_interval[2])
)

#dataset for plot
plot_data <- df_percent %>%
  mutate(Level = RemoteWork) %>%
  mutate(Predicted = ifelse(Year == 2025, TRUE, FALSE)) %>%
  bind_rows(
    data.frame(
      Year = 2025,
      RemoteWork = c("Remote", "Hybrid", "In-person"),
      Count = NA,
      Percentage = c(remote_analysis$prediction, 
                     hybrid_analysis$prediction, 
                     inperson_analysis$prediction),
      Level = c("Remote", "Hybrid", "In-person"),
      Predicted = TRUE
    )
  )

#plot 1 (divided cases)
ggplot(plot_data, aes(x = Year, y = Percentage, color = Level)) +
  geom_point(data = plot_data %>% filter(!Predicted), size = 2) +
  geom_line(data = plot_data %>% filter(!Predicted), aes(group = Level), size = 1) +
  geom_line(data = plot_data %>% filter(Year >= 2024),
            aes(group = Level),
            linetype = "dashed", size = 1) +  # Linea tratteggiata dal 2024 al 2025
  geom_point(data = plot_data %>% filter(Predicted), size = 3, shape = 17) +
  geom_errorbar(data = plot_data %>% filter(Year == 2025),
                aes(ymin = c(remote_analysis$conf_interval[1],
                             hybrid_analysis$conf_interval[1],
                             inperson_analysis$conf_interval[1]),
                    ymax = c(remote_analysis$conf_interval[2],
                             hybrid_analysis$conf_interval[2],
                             inperson_analysis$conf_interval[2])),
                width = 0.2, size = 1) +  # Intervallo di confidenza per il 2025
  geom_text(data = plot_data %>% filter(Year == 2025),
            aes(x = 2025, 
                y = c(remote_analysis$prediction, 
                      hybrid_analysis$prediction, 
                      inperson_analysis$prediction),
                label = round(c(remote_analysis$prediction, 
                                hybrid_analysis$prediction, 
                                inperson_analysis$prediction), 2),
                color = c("Remote", "Hybrid", "In-person")),  # Colore associato alla categoria
            vjust = -0.5, hjust = 1.1, size = 4) +  # Testo vicino alla previsione
  facet_wrap(~Level, scales = "free_y", ncol = 3) +
  scale_y_continuous(breaks = seq(0, 100, by = 2.5)) +  # Unifica la scala dell'asse y con intervallo di 2.5
  labs(title = "Prediction of Work Modes for 2025",
       x = "Year", y = "Percentage (%)",
       color = "Work Mode") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


#plot 2 (all together)
ggplot(plot_data, aes(x = Year, y = Percentage, color = Level)) +
  geom_point(data = plot_data %>% filter(!Predicted), size = 2) +
  geom_line(data = plot_data %>% filter(!Predicted), aes(group = Level), size = 1) +
  geom_line(data = plot_data %>% filter(Year >= 2024),
            aes(group = Level),
            linetype = "dashed", size = 1) +
  geom_point(data = plot_data %>% filter(Predicted), size = 3, shape = 17) +
  geom_errorbar(data = plot_data %>% filter(Year == 2025),
                aes(ymin = c(remote_analysis$conf_interval[1],
                             hybrid_analysis$conf_interval[1],
                             inperson_analysis$conf_interval[1]),
                    ymax = c(remote_analysis$conf_interval[2],
                             hybrid_analysis$conf_interval[2],
                             inperson_analysis$conf_interval[2])),
                width = 0.2, size = 1) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Prediction of Work Modes for 2025",
       x = "Year", y = "Percentage (%)",
       color = "Work Mode") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
  

#plot the values of the conformal predictions
conf_table <- data.frame(
  Level = c("Remote", "Hybrid", "In-person"),
  Prediction2025 = c(remote_analysis$prediction, 
                     hybrid_analysis$prediction, 
                     inperson_analysis$prediction),
  LowerBound = c(remote_analysis$conf_interval[1], 
                 hybrid_analysis$conf_interval[1], 
                 inperson_analysis$conf_interval[1]),
  UpperBound = c(remote_analysis$conf_interval[2], 
                 hybrid_analysis$conf_interval[2], 
                 inperson_analysis$conf_interval[2])
)

grid.newpage()
grid.table(conf_table)


