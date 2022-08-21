### Transforming occurrence data of species in an abundance matrix
library(dplyr)

### reading data 
sp_sc <- read.csv("data/raw/occurrence_sp_sc.csv")
sp_sc_use_noNA <- read.csv("data/processed/occurrence_sp_sc_use_noNA.csv")

### Transforming occurrence data of "all species" in an abundance matrix
table_abund_sp <- sp_sc %>% 
  select(scrubbed_species_binomial) %>% 
  table ()

### Transforming table in a data frame so we can merge all species abundance df
### with cultivated species abundance df

abund_sp <- as.data.frame(table_abund_sp)

### Transforming occurrence data of "cultivated species" in an abundance matrix
table_abund_cultivated <- sp_sc_use_noNA %>% 
  select(scrubbed_species_binomial) %>% 
  table ()

### transforming table in a data frame so we can merge all species abundance df
### with cultivated species abundance df

abund_cultivated <- as.data.frame(table_abund_cultivated)

### Changing the name of columns
colnames(abund_sp) <- c("Specie", "Freq")
colnames(abund_cultivated) <- c("Specie", "Freq")

### Saving processed data of abundance matrix
write.csv(x = abund_sp,
          file = "data/processed/abund_sp.csv",
          row.names = FALSE)

write.csv(x = abund_cultivated,
          file = "data/processed/abund_cultivated.csv",
          row.names = FALSE)

### sort by most frequent
sorted_all <- sort(table_abund_sp, decreasing = TRUE)
sorted_cult <- sort(table_abund_cultivated, decreasing = TRUE)

### transforming table in data frame
sorted_all <- as.data.frame(sorted_all)
sorted_cult <- as.data.frame(sorted_cult)

### selecting the first 50 more frequent
sorted_50_all <- sorted_all[1:50,]
sorted_50_cult <- sorted_cult[1:50,]

write.csv(x = sorted_50_all,
          file = "data/processed/all_50.csv",
          row.names = F)

write.csv(x = sorted_50_cult,
          file = "data/processed/cult_50.csv")

### Joining both categories with the 50 most abundant species
total_abund_50 <- dplyr::full_join(
  sorted_50_all,
  sorted_50_cult,
  by = "."
)

### renaming columns of total abundance data frame
colnames(total_abund_50) <- c("specie","freq. all", "freq. cultivated")

### changing NA to zero
total_abund_50[is.na(total_abund_50)] <- 0

write.csv(x = total_abund_50,
          file = "data/processed/total_abund_50.csv",
          row.names = F)
