library(dplyr)
library(ggplot2)
set.seed(19)

data_scientist <- read.csv("reduced_dataset.csv",header=T)
data_filtered <- data_scientist %>% filter(!is.na(ConvertedCompYearly))

summary(aov(ConvertedCompYearly~EdLevel+OrgSize ,data = data_filtered))
#WORKFLOW
#COLUMNS TO REMOVE A PRIORI --------------

#RemoteWork -> X
#PurchInfluence -> X
#age -> X

#ASSESS SIGNIFICANCY OF CATEGORICAL VARIABLES WITH ANOVA ------------

#SPLIT EUROPE AND AMERICA+CANADA!

#EdLevel
#OrgSize
#IcorPM (TEST DI IPOTESI)

#REGRESSION WITH SIGNIFICANT VARIABLES USING LAB7+LAB8 (GAM) METHODS ---------------

#Remove outliers? (using bagplot: income-work experience)

#Country (random intercept?)
#Work experience
#Significant Categorical Variables 

#find combination for GAM (look lab.8)

#Comparison of Europe and America regression lines (no intercept)


#QUANTILE REGRESSION -------------