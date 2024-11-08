library(dplyr)
library(ggplot2)

set.seed(19)
#load data
data_scientist <- read.csv("reduced_dataset.csv",header=T)

#FILTERING DATASET -------------
#we filter the data keeping just people in range 25-34 years old who got a Master Degree and in countries in which there
#are at least 5 observations
data_filtered <- data_scientist %>%
  # Filtra per valori non NA in ConvertedCompYearly
  filter(!is.na(ConvertedCompYearly) & EdLevel == 'Master’s degree (M.A., M.S., M.Eng., MBA, etc.)' & Age == "25-34 years old" 
        # & Country != 'Austria' & Country != 'Poland')
        )%>%
  # Conta le occorrenze per ogni paese e aggiungi come colonna temporanea `count`
  group_by(Country) %>%
  mutate(count = n()) %>%
  # Filtra solo i paesi con almeno 10 occorrenze
  filter(count >= 5) %>%
  # Rimuovi la colonna temporanea `count`
  ungroup() #%>%
  #select(-count)

#change name of these countries just for better plots
data_filtered <- data_filtered %>%
  mutate(Country = ifelse(Country == "United Kingdom of Great Britain and Northern Ireland", "UK", Country))

data_filtered <- data_filtered %>%
  mutate(Country = ifelse(Country == "United States of America", "USA", Country))

#dataset with just the yearly compensation for each nation
stipendi <- data_filtered[,c(6,9)]

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
  scale_fill_brewer(palette = "Set3")  # Palette di colori per differenziare i paesi

#DISTRIBUTION COMPARISON ---------------
#function to perform permutation test
perm_t_test=function(x,y,iter=1e3){
  
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
data <- data.frame(Country = c('Austria',"Denmark","France","Germany","Netherlands",'Polonia',"Portugal","Spain","Sweden","Switzerland","UK","USA"),p_value =p_vals)
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
#bootstrap confidence intervals per le mediane/delta mediane?