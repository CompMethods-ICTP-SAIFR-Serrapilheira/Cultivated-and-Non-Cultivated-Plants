# Exploring data from queries in BIEN data base.
library(BIEN)
library(tidyr)
# Occurrence of species in the state of Santa Catarina
sp_sc <-BIEN_occurrence_state(country = "Brazil",
                              state = "Santa Catarina")

if (!dir.exists("data/raw/")) dir.create("data/raw/")
write.csv(x = sp_sc,
          file = "data/raw/occurrence_sp_sc.csv",
          row.names = FALSE)

# Occurrence of cultivated species in the state of Santa Catarina
sp_sc_use <-BIEN_occurrence_state(country = "Brazil",
                                  state = "Santa Catarina",
                                  cultivated = TRUE)


write.csv(x = sp_sc_use,
          file = "data/raw/occurrence_sp_sc_use.csv",
          row.names = FALSE)

# Exploring data
head(sp_sc)
head(sp_sc_use)
unique(sp_sc$datasource)
sp_sc_use$is_location_cultivated
length(unique(sp_sc$scrubbed_species_binomial))
length(unique(sp_sc_use$scrubbed_species_binomial))

# Removing the rows with NA value in "Is_cultivated_observation"
sp_sc_use_noNA <- sp_sc_use %>% drop_na(is_cultivated_observation)

if (!dir.exists("data/processed/")) dir.create("data/processed/")
write.csv(x = sp_sc_use_noNA,
          file = "data/processed/occurrence_sp_sc_use_noNA.csv",
          row.names = FALSE)

# Exploring data of cultivated species
length(sp_sc_use_noNA$scrubbed_species_binomial)
length(unique(sp_sc_use_noNA$scrubbed_species_binomial))
