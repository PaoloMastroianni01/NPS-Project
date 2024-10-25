#LIBRARIES ------------
library('readxl')

#PREPROCESSING -----------
#load data
data_scientist <- read.csv('survey_results_public.csv',header=T)

#countries
countries <- c(
  "Albania",
  "Andorra",
  "Armenia",
  "Austria",
  "Azerbaijan",
  "Belarus",
  "Belgium",
  "Bosnia and Herzegovina",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czech Republic",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Georgia",
  "Germany",
  "Greece",
  "Hungary",
  "Iceland",
  "Ireland",
  "Italy",
  "Kazakhstan",
  "Latvia",
  "Liechtenstein",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Moldova",
  "Monaco",
  "Montenegro",
  "Netherlands",
  "North Macedonia",
  "Norway",
  "Poland",
  "Portugal",
  "Romania",
  "Russia",
  "San Marino",
  "Serbia",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "Switzerland",
  "Turkey",
  "Ukraine",
  "United Kingdom of Great Britain and Northern Ireland",
  "Vatican City",
  'United States of America',
  'Canada'
)

#keep just data scientists who work full-time in Europe or US/Canada
data_scientist <- data_scientist[data_scientist$DevType %in% c('Data scientist or machine learning specialist') &
                                   data_scientist$MainBranch %in% c('I am a developer by profession') & 
                                   data_scientist$Employment %in% c('Employed, full-time') &
                                   data_scientist$Country %in% countries, ]

#we add the corresponding number to each column to easily find the useless ones and remove them 
#colnames(data_scientist) <- paste(colnames(data_scientist), seq_along(colnames(data_scientist)), sep = "_")
#remove useless columns
data_scientist <- data_scientist[,-c(1,2,3,5,7,9:14,17,18,23:64,67:83)]

#count and remove the lines with NA
na_count <- sapply(data_scientist, function(x) sum(is.na(x)))
data.frame(NA_Count = na_count)
#data_scientist_clean <- data_scientist[complete.cases(data_scientist$CompTotal_21), ]

#change in factor columns
data_scientist[c(1,2,3,4,5,6,10)] <- lapply(data_scientist[c(1,2,3,4,5,6,10)], as.factor)
str(data_scientist)

# Passo 1: Contare le occorrenze di ciascuna nazione
country_counts <- table(data_scientist$Country)
# Passo 2: Identificare le nazioni con meno di 10 occorrenze
countries_to_remove <- names(country_counts[country_counts < 10])
# Passo 3: Rimuovere le osservazioni relative a quelle nazioni
data_scientist_clean <- data_scientist[!(data_scientist$Country %in% countries_to_remove), ]

# target languages
target_languages <- c("C", "C#", "C++", "Julia", "MATLAB", "Python", "R", "Scala", "SQL")
# Build empty auxiliary dataset in which for each observation we write 1 if it uses that language, no otherwise
language_df <- as.data.frame(matrix(0, nrow = nrow(data_scientist), ncol = length(target_languages)))
colnames(language_df) <- target_languages

for (i in 1:nrow(data_scientist)) {
  # Extract languages
  languages_present <- unlist(strsplit(data_scientist$LanguageHaveWorkedWith[i], ";"))
  # check presence o target languages
  for (lang in target_languages) {
    if (lang %in% languages_present) {
      language_df[i, lang] <- 1
    }
  }
}

#add auxiliary dataset to the original one and remove it
data_scientist <- cbind(data_scientist, language_df)
rm(language_df)

#remove column with all used languages
data_scientist <- data_scientist[,-9]


