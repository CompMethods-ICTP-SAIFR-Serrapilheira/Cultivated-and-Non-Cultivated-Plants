### now that we have a data frame with the 50 most abundant species in each category
### we will use the package BIEN to get trait data of these species
library(BIEN)

### to do the queries in BIEN data base, we need the vectors with the name of the
### first 50 species from each category
abund_all_50 <- read.csv("data/processed/all_50.csv")
abund_cult_50 <- read.csv("data/processed/cult_50.csv")

### taking off the first column of cultivated df
abund_cult_50 <- abund_cult_50[,-1]

### putting species into a vector to do the requet in BIEN db
all_50 <- as.vector(abund_all_50$.)
cult_50 <- as.vector(abund_cult_50$.)

### requesting trait data of the most abundant species
trait_all <- BIEN_trait_species(all_50)
length(unique(trait_all$scrubbed_species_binomial)) # probably there isn't trait information for all species, that would explain the unique length being smaller than 50

### requesting trait data of the most abundant cultivated species
trait_cult <- BIEN_trait_species(cult_50)
length(unique(trait_cult$scrubbed_species_binomial)) # probably there isn't trait information for all species, that would explain the unique length being smaller than 50

write.csv(x = trait_all,
          file = "data/raw/trait_all_sp.csv",
          row.names = F)
write.csv(x = trait_cult,
          file = "data/raw/trait_cultivated_sp.csv",
          row.names = F)
