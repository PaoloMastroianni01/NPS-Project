library(dplyr)
library(arules) #association rules

#LOAD DATA
data_scientist <- read.csv("reduced_dataset.csv",header=T)

#A PRIORI ALGORITHM FOR PROGRAMMING LANGUAGES -----------------
# Crea una matrice binaria con linguaggi usati
languages <- data_scientist %>% select(C..,Julia,MATLAB,Python,R,Scala,SQL) %>%
  filter(rowSums(.) > 0)  # Mantieni righe con almeno un linguaggio usato

languages <- languages %>%
  mutate(across(everything(), ~ . == 1))

# Converti in formato transazionale
transactions <- as(languages, "transactions")

# Regole di associazione
rules <- apriori(transactions, parameter = list(supp = 0.1, conf = 0.1))  #

rules_lift_gt_1 <- subset(rules, subset = lift > 1)

# Visualizza le regole
inspect(rules)
inspect(rules_lift_gt_1)

#EVOLUTION OF REMOTE/IN-PERSON/HYBRID --------------