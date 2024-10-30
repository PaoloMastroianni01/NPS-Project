#LIBRARIES ------------
library(readxl)
library(DepthProc)
library(aplpack)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

#read file
data_scientist <- read.csv("reduced_dataset.csv",header=T)

#PREPROCESSING -----------------------
#transform in factor variables
data_scientist <- data_scientist[,-c(10,11)] #remove C and C#
data_scientist[1:7] <- lapply(data_scientist[c(1:7)], as.factor)
data_scientist[10:16] <- lapply(data_scientist[c(10:16)], as.factor)
names(data_scientist)[names(data_scientist) == "C.."] <- "Cplusplus" #because ++ gives issues

#remove people who are paid less than guys asking elemosina al semaforo
data_scientist <- data_scientist[-which(data_scientist$ConvertedCompYearly<6000),]
#remove Elon Musk
data_scientist <- data_scientist[-which(data_scientist$ConvertedCompYearly>750000),]
#remove people who don't say their age
data_scientist <- data_scientist[-which(data_scientist$Age=='Prefer not to say'),]
#we will focus only on people who at least attended university
data_scientist <- data_scientist[-which(data_scientist$EdLevel=='Associate degree (A.A., A.S., etc.)' |
                                        data_scientist$EdLevel=='Primary/elementary school' |
                                        data_scientist$EdLevel=='Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)'|
                                        data_scientist$EdLevel=='Something else'),]
#remove people who don't know the size of their company and those ones who are not in a company
data_scientist <- data_scientist[-which(data_scientist$OrgSize=='I don’t know' |
                                          data_scientist$OrgSize=='Just me - I am a freelancer, sole proprietor, etc.'),]

#count NA for each variable
na_count <- sapply(data_scientist, function(x) sum(is.na(x)))
data.frame(NA_Count = na_count)
rm(na_count)

#DATA VISUALIZATION ----------
cols <- names(data_scientist)
cols <- cols[-c(6,8,9)]

#for each variable of interest, we plot it against the yearly compensation
for(col in cols){
attach(data_scientist)
data_filtered <- data_scientist[!is.na(ConvertedCompYearly) & !is.na(col), ]
detach(data_scientist)
p <- ggplot(data_filtered, aes_string(x = col , y =' ConvertedCompYearly', fill = col)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) + # Aggiungi boxplot con outlier
  scale_x_discrete(labels=NULL,drop = T) +
  scale_y_continuous(labels = scales::comma) +  # Formatta l'asse y con le virgole per i grandi numeri
  labs(title = paste("Annual Salary for", col),
       x = paste(col),
       y = "Salario Annuale (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_brewer(palette = "Set3")
print(p)
}
rm(data_filtered)
rm(cols)
rm(col)
rm(p)
#what about italy
#filter data
data_europe <- data_scientist %>%
  filter(!is.na(ConvertedCompYearly) & Country != "United States of America" & Country != "Canada" ) %>%
  mutate(Region = ifelse(Country == "Italy", "Italy", "Other Europe"))
data_america <- data_scientist %>%
  filter(!is.na(ConvertedCompYearly) & Country == "United States of America" | Country == "Canada" | Country == "Italy" ) %>%
  mutate(Region = ifelse(Country == "Italy", "Italy", "America"))
data_experience <- data_scientist %>%
  filter(!is.na(ConvertedCompYearly) & !is.na(WorkExp))

#italy vs europe
ggplot(data_europe, aes(x = Region, y = ConvertedCompYearly, fill = Region)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Annual Salary: Italy vs Other European Countries",
       x = "Country",
       y = "Annual Salary (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_brewer(palette = "Set1")  # Palette di colori per differenziare i paesi
rm(data_europe)

#italy vs US+Canada
ggplot(data_america, aes(x = Country, y = ConvertedCompYearly, fill = Country)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Annual Salary: Italy vs United States and Canada",
       x = "Country",
       y = "Annual Salary (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_brewer(palette = "Set2")  # Palette di colori per differenziare i paesi
rm(data_america)

#compensation over years of experience and in color put the categorical variable you want
ggplot(data_experience, aes(x = WorkExp, y = ConvertedCompYearly)) +
  geom_jitter(aes(color = EdLevel), width = 0.2, alpha = 0.6) +  # Punti di esperienza
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed", size = 1.2) +  # Linea di regressione
  geom_hline(yintercept = median(data_scientist$ConvertedCompYearly,na.rm=T),color = "forestgreen", size=1.2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Annual Salary by Years of Experience",
       x = "Years of Experience",
       y = "Annual Salary (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_color_brewer(palette = "Set1") # Colori per evidenziare gli anni di esperienza
rm(data_experience)


#depth measures + bagplot
data_filtered <- data_scientist %>% filter(!is.na(ConvertedCompYearly) & !is.na(WorkExp))

depth_values <- depthContour(cbind(data_filtered$WorkExp, data_filtered$ConvertedCompYearly), 
                             depth_params = list(method = "Tukey"))

bagplot_data <- bagplot(cbind(data_filtered$WorkExp, data_filtered$ConvertedCompYearly), 
                        factor = 3, show.whiskers = TRUE)
outliers <- bagplot_data$pxy.outlier
outliers

#NOMPARAMETRIC INFERENCE --------------
