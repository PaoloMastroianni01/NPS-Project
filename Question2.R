library(dplyr)
library(ggplot2)
set.seed(19)

#anova + manova 
data_scientist <- read.csv("reduced_dataset.csv",header=T)
data_filtered <- data_scientist %>% filter(!is.na(ConvertedCompYearly))

#REMOVE:
#age -> X
summary(aov(ConvertedCompYearly~EdLevel+OrgSize ,data = data_filtered))

#RemoteWork -> X
#PurchInfluence -> X

#ANOVA 
#EdLevel -> yes
#OrgSize -> yes
#IcorPM -> yes (TEST DI IPOTESI)

#REGRESSION WITH SIGNIFICANT VARIABLES
#Country -> yes
#workexp -> ok
#compyearly -> response

#splines con anni di esperienza 
#
