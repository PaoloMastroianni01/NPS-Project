library(dplyr)

#load data
data_scientist_2022 <- read.csv("reduced_dataset_2022.csv",header=T)
data_scientist_2023 <- read.csv("reduced_dataset_2023.csv",header=T)
data_scientist_2024 <- read.csv("reduced_dataset_2024.csv",header=T)

#we will focus only on these nations
data_scientist_2022 <- data_scientist_2022[data_scientist_2022$Country %in% c('Austria','Belgio','Canada','Denmark','France','Germany','Italy',
                                                                              'Netherlands','Poland','Portugal','Spain','Sweden','Switzerland',
                                                                              'United Kingdom of Great Britain and Northern Ireland','United States of America'),]

data_scientist_2023 <- data_scientist_2023[data_scientist_2023$Country %in% c('Austria','Belgio','Canada','Denmark','France','Germany','Italy',
                                                                              'Netherlands','Poland','Portugal','Spain','Sweden','Switzerland',
                                                                              'United Kingdom of Great Britain and Northern Ireland','United States of America'),]

data_scientist_2024 <- data_scientist_2024[data_scientist_2024$Country %in% c('Austria','Belgio','Canada','Denmark','France','Germany','Italy',
                                                                              'Netherlands','Poland','Portugal','Spain','Sweden','Switzerland',
                                                                              'United Kingdom of Great Britain and Northern Ireland','United States of America'),]

data_scientist_2022$Year <- 2022
data_scientist_2023$Year <- 2023
data_scientist_2024$Year <- 2024

#inflation factors for 2022 and 2023
inflation_factors2022 <- c(
  Austria = 1.170, Belgium = 1.148, Canada = 1.108, Denmark = 1.125,
  France = 1.113, Germany = 1.153, Italy = 1.147,
  Netherlands = 1.180, Poland = 1.276, Portugal = 1.136, Spain = 1.121,
  Sweden = 1.159, Switzerland = 1.049,
  `United Kingdom of Great Britain and Northern Ireland` = 1.177,
  `United States of America` = 1.124
)

inflation_factors2023 <- c(
  Austria = 1.078, Belgium = 1.046, Canada = 1.037, Denmark = 1.046,
   France = 1.047, Germany = 1.061, Italy = 1.055,
  Netherlands = 1.057, Poland = 1.114, Portugal = 1.054, Spain = 1.033,
  Sweden = 1.068, Switzerland = 1.021,
  `United Kingdom of Great Britain and Northern Ireland` = 1.078,
  `United States of America` = 1.042
)

# Correggere il salario per il 2022
data_scientist_2022$ConvertedCompYearly <- 
  data_scientist_2022$ConvertedCompYearly * 
  sapply(data_scientist_2022$Country, function(country) inflation_factors2022[[country]])

# Correggere il salario per il 2023
data_scientist_2023$ConvertedCompYearly <- 
  data_scientist_2023$ConvertedCompYearly * 
  sapply(data_scientist_2023$Country, function(country) inflation_factors2023[[country]])

data_scientist <- rbind(data_scientist_2022,data_scientist_2023,data_scientist_2024)

write.csv(data_scientist,'reduced_dataset.csv')
