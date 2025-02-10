library(dplyr)
library(ggplot2)
library(RColorBrewer)

set.seed(19)
#load data
data_scientist <- read.csv("reduced_dataset.csv",header=T)

#FILTERING DATASET -------------
data_filtered <- data_scientist %>%
  # Filtra per valori non NA in ConvertedCompYearly
  filter(!is.na(ConvertedCompYearly) & WorkExp <= 15)

#change name of these countries just for better plots
data_filtered <- data_filtered %>%
  mutate(Country = ifelse(Country == "United Kingdom of Great Britain and Northern Ireland", "UK", Country))

data_filtered <- data_filtered %>%
  mutate(Country = ifelse(Country == "United States of America", "USA", Country))

#dataset with just the yearly compensation for each nation
stipendi <- data_filtered[,c(5,8)]

palette_15 <- colorRampPalette(brewer.pal(12, "Set3"))(15)
ggplot(stipendi, aes(x = Country, y = ConvertedCompYearly, fill = Country)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  coord_cartesian(ylim=c(0,350000)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Annual Salary: Italy vs Other Countries",
       x = "Country",
       y = "Annual Salary (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  ) +
  scale_fill_manual(values = palette_15)

#DISTRIBUTION COMPARISON ---------------
#function to perform permutation test
perm_t_test=function(x,y,iter=1e4){
  
  T0=abs(median(x)-median(y))  # define the test statistic
  T_stat=numeric(iter) # a vector to store the values of each iteration
  x_pooled=c(x,y) # pooled sample
  n=length(x_pooled)
  n1=length(x)
  for(perm in 1:iter){ # loop for conditional MC
    # permutation:
    permutation <- sample(1:n)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1]
    x2_perm <- x_perm[(n1+1):n]
    # test statistic:
    T_stat[perm] <- abs(median(x1_perm) - median(x2_perm))
  }
  
  # p-value
  p_val <- sum(T_stat>=T0)/iter
  return(p_val)
}
p_vals <- c()
#we transform in factor each country and we perform a permutation for each country vs italy to assess if equality
#in distribution can be assumed 
data_filtered$Country <- as.factor(data_filtered$Country)
for(nation in levels(data_filtered$Country)){
  if(nation!= as.character('Italy')){
    p.value <- perm_t_test(data_filtered$ConvertedCompYearly[data_filtered$Country=='Italy'],data_filtered$ConvertedCompYearly[data_filtered$Country==nation],iter=1e3)
    cat('italy vs ',as.character(nation),'p-value: ',p.value,'\n')
    p_vals <- c(p_vals,p.value)
}
}

#build dataset with p-values of each country and plot them
data <- data.frame(Country = c('Austria','Belgium','Canada',"Denmark","France","Germany","Netherlands",'Polonia',"Portugal","Spain","Sweden","Switzerland","UK","USA"),p_value =p_vals)
data <- data %>%
  mutate(p_value_category = case_when(
    p_value < 0.05 ~ "< 0.05",
    p_value >= 0.05 & p_value < 0.1 ~ "0.05<x<0.1",
    p_value >= 0.1 ~ ">= 0.1"
  ))
ggplot(data, aes(x = reorder(Country, p_value), y = p_value)) +
  geom_bar(stat = "identity", aes(fill = p_value_category)) +
  scale_fill_manual(values = c("< 0.05" = "red", "0.05<x<0.1" = "tomato", ">= 0.1" = "green")) +
  labs(x = "Country", y = "p-value", title = "Italy vs Other Countries p-values") +
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = .05, linetype = "dashed", color = "red") +
  geom_hline(yintercept = .1, linetype = "dashed", color = "red") +
  scale_y_continuous(breaks = c(0,0.05, 0.1, 1))
#BOOTSTRAP ------------

#Prepare a list to save results
results <- data.frame(
  Country = character(),
  Median = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)


n_bootstrap <- 10000

#Functions that computes n_bootstrap datasets, compute its median and then gives a confidence interval for the median of that country
bootstrap_ci <- function(values, n_bootstrap, conf_level = 0.95) {
  n <- length(values)
  boot_medians <- replicate(n_bootstrap, {
    sample_data <- sample(values, size = n, replace = TRUE)
    median(sample_data)
  })
  alpha <- (1 - conf_level) / 2
  ci <- quantile(boot_medians, probs = c(alpha, 1 - alpha))
  list(lower = ci[1], upper = ci[2])
}

for (country in unique(stipendi$Country)) {
  data_boot <- stipendi %>% 
    filter(!is.na(ConvertedCompYearly) & Country == country)
  
  #compute sample median and confidence interval
  ci <- bootstrap_ci(data_boot$ConvertedCompYearly, n_bootstrap)
  median_sample <- median(data_boot$ConvertedCompYearly, na.rm = TRUE)
  
  #add results to data frame
  results <- rbind(
    results, 
    data.frame(
      Country = country,
      Median = median_sample,
      CI_Lower = ci$lower,
      CI_Upper = ci$upper
    )
  )
}

print(results)

ggplot(results, aes(x = Country, y = Median)) +
  geom_point(color = "blue") + 
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  #coord_flip() +
  labs(
    title = "Confidence Intervals for Median by Country",
    x = "Country",
    y = "Median and Confidence Interval"
  ) +
  geom_hline(yintercept = results[results$Country=='Italy',3], linetype = "dashed", color = "red") +
  geom_hline(yintercept = results[results$Country=='Italy',4], linetype = "dashed", color = "red") +
  theme_minimal()

table(stipendi$Country)

