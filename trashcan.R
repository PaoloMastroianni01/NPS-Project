#nations with at least X observations ------------

# Passo 1: Contare le occorrenze di ciascuna nazione
country_counts <- table(data_scientist$Country)
# Passo 2: Identificare le nazioni con meno di 10 occorrenze
countries_to_remove <- names(country_counts[country_counts < 10])
# Passo 3: Rimuovere le osservazioni relative a quelle nazioni
data_scientist_clean <- data_scientist[!(data_scientist$Country %in% countries_to_remove), ]
