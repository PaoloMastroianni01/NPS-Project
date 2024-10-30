library(readxl)
library(DepthProc)
library(aplpack)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(dplyr)

#read file
data_scientist <- read.csv("reduced_dataset.csv",header=T)

#columns we want to plot
cols <- names(data_scientist)
cols <- cols[-c(6,8,9:16)]
vec <- c(1:5,7)

#for each variable of interest, we plot it against the yearly compensation
for(idx in 1:length(cols)){
  col <- cols[idx]
  i <- vec[idx]
data_filtered <- data_scientist[!is.na(data_scientist$ConvertedCompYearly) & !is.na(data_scientist[,i]), ]
p <- ggplot(data_filtered, aes_string(x = col , y =' ConvertedCompYearly', fill = col)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) + # Aggiungi boxplot con outlier
  scale_x_discrete(labels=NULL,drop = T) +
  scale_y_continuous(labels = scales::comma) +  # Formatta l'asse y con le virgole per i grandi numeri
  labs(title = paste("Annual Salary for", col),
       x = paste(col),
       y = "Annual Salary (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  ) +
  scale_fill_brewer(palette = "Set3")
print(p)
}
rm(data_filtered)
rm(cols)
rm(col)
rm(p)

#histograms for each programming language (occurencies and boxplot for annual salary)
cols <- names(data_scientist)
cols <- cols[c(10:16)]


attach(data_scientist)
for (col_name in cols) {
  data_filtered <- data_scientist[!is.na(ConvertedCompYearly) & !is.na(col), ]
  #histogram occurencies (consider also observations who didnt say their salary)
  p1 <- ggplot(data_scientist, aes_string(x = col_name)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = paste("Occurrencies for", col_name),
         x = col_name,
         y = "Frequencies") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Filter data
  data_0 <- data_filtered[data_filtered[[col_name]] == 0, ]
  data_1 <- data_filtered[data_filtered[[col_name]] == 1, ]
  
  # Boxplot per chi ha col_name = 0
  p2a <- ggplot(data_0, aes(y = ConvertedCompYearly)) +
    geom_boxplot(fill = "red", color = "black") +
    labs(title = paste("Annual Salary for", col_name, "NO"),
         x = col_name,
         y = "Annual Salary (USD)") +
    theme_minimal()
  
  # Boxplot per chi ha col_name = 1
  p2b <- ggplot(data_1, aes(y = ConvertedCompYearly)) +
    geom_boxplot(fill = "lightgreen", color = "black") +
    labs(title = paste("Annual Salary for", col_name, "YES"),
         x = col_name,
         y = "Annual Salary (USD)") +
    theme_minimal()
  
  # Combina i due grafici in un layout a due colonne
  grid.arrange(p1, p2a, p2b, ncol = 3)
}
detach(data_scientist)

rm(col)
rm(col_name)
rm(cols)
rm(p)
rm(p1)
rm(p2)
rm(p2a)
rm(p2b)
rm(data_0)
rm(data_1)
rm(data_filtered)

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
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
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
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  ) +
  scale_fill_brewer(palette = "Set2")  # Palette di colori per differenziare i paesi
rm(data_america)

#compensation over years of experience and in color put the categorical variable you want
ggplot(data_experience, aes(x = WorkExp, y = ConvertedCompYearly)) +
  geom_jitter(aes(color = WorkExp), width = 0.2, alpha = 0.6) +  # Punti di esperienza
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed", size = 1.2) +  # Linea di regressione
  #geom_hline(yintercept = median(data_scientist$ConvertedCompYearly,na.rm=T),color = "forestgreen", size=1.2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Annual Salary by Years of Experience",
       x = "Years of Experience",
       y = "Annual Salary (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    #legend.text = element_text(size = 12),
    #legend.title = element_text(size = 12)
    legend.position = 'none'
  ) +
  #scale_color_brewer(palette = "Set1") # Colori per evidenziare gli anni di esperienza
  scale_color_gradient(low = "lightblue", high = "blue")
rm(data_experience)


#depth measures + bagplot
data_filtered <- data_scientist %>% filter(!is.na(ConvertedCompYearly) & !is.na(WorkExp))

depth_values <- depthContour(cbind(data_filtered$WorkExp, data_filtered$ConvertedCompYearly), 
                             depth_params = list(method = "Tukey"))

bagplot_data <- bagplot(cbind(data_filtered$WorkExp, data_filtered$ConvertedCompYearly), 
                        factor = 3, show.whiskers = TRUE)
outliers <- bagplot_data$pxy.outlier
outliers
