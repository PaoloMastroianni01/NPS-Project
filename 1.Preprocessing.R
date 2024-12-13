#preprocessing on 2023 dataset. For 2022 and 2024 it was basically the same besides some indexes changings
#load data
data_scientist <- read.csv('survey_results_public.csv',header=T)

#countries we will focus on (Europe+US+Canada)
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

#remove useless columns
data_scientist <- data_scientist[,-c(1,2,3,5,7,9:14,17,18,23:64,67:83)]

#count the lines with NA
na_count <- sapply(data_scientist, function(x) sum(is.na(x)))
data.frame(NA_Count = na_count)
#data_scientist_clean <- data_scientist[complete.cases(data_scientist$CompTotal_21), ]

#change in factor columns
data_scientist[c(1,2,3,4,5,6,10)] <- lapply(data_scientist[c(1,2,3,4,5,6,10)], as.factor)
str(data_scientist)

# target languages
target_languages <- c("C++", "Julia", "MATLAB", "Python", "R", "Scala", "SQL")

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
data_scientist <- data_scientist[,-c(7,8,9)]

#transform in factor variables
data_scientist[10:16] <- lapply(data_scientist[c(10:16)], as.factor)
names(data_scientist)[names(data_scientist) == "C.."] <- "Cplusplus" #because ++ gives issues

#remove people who have less than 1000 dollary per month salary
data_scientist <- data_scientist[-which(data_scientist$ConvertedCompYearly<10000),]
#remove non-sense salary
data_scientist <- data_scientist[-which(data_scientist$ConvertedCompYearly>2000000),]
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

#remove columns we assessed (during analysis) to be meaningless
data_scientist <- data_scientist[,-c(5,7)]

#redefine organization sizes
library(dplyr)
redefine_orgsize <- function(df) {
  df %>%
    mutate(
      OrgSize = recode_factor(
        OrgSize,
        "2 to 9 employees" = "2 to 99",
        "10 to 19 employees" = "2 to 99",
        "20 to 99 employees" = "2 to 99",
        "100 to 499 employees" = "100 to 999",
        "500 to 999 employees" = "100 to 999",
        "1,000 to 4,999 employees" = "1000 to 9999",
        "5,000 to 9,999 employees" = "1000 to 9999",
        "10,000 or more employees" = "10000 or more"
      ),
      #Set order of levels
      OrgSize = factor(OrgSize, levels = c(
        "2 to 99", "100 to 999", "1000 to 9999", "10000 or more"
      ))
    )
}
data_scientist <- redefine_orgsize(data_scientist)

#create csv file with reduced dataset
write.csv(data_scientist, "reduced_dataset2023.csv", row.names = FALSE)











