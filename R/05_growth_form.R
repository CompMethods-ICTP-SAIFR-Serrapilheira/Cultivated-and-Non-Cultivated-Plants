### In this script we are going to try to understand if there are some differences
### in specific traits when comparing the two categories (all species and cultivated)
### it is expected that cultivated species will present less diversity on traits
### and that the biggest part of it's species will be represented by few traits
### the trait that is being explored is the whole plant growth form

library(dplyr)
library(ggplot2)

### we need to work with the functional traits data frame, so it can be cleaner
trait_all <- read.csv("data/raw/trait_all_sp.csv")
trait_cult <- read.csv("data/raw/trait_cultivated_sp.csv")

### leaving only information of species, traits and trait values
trait_all <- trait_all[,1:3]
trait_cult <- trait_cult[,1:3]

### checking trait names in the data frame
unique(trait_all$trait_name)
head(trait_all)

### removing duplicated data
trait_all <- unique(trait_all)
trait_cult <- unique(trait_cult)

# transposing df of "trait_all"
# sp_all <- as.vector(trait_all$scrubbed_species_binomial)
# trait_name_all <- as.vector(trait_all$trait_name)
# trait_val_all <- as.vector(trait_all$trait_value)
# 
# t_trait_all <- data.frame(rbind(sp_all,trait_name_all, trait_val_all))
# colnames(t_trait_all) <- t_trait_all[1,]
# t_trait_all <- t_trait_all[-1,]
# 
# # transposing df of "trait_cult"
# sp_cult <- as.vector(trait_cult$scrubbed_species_binomial)
# trait_name_cult <- as.vector(trait_cult$trait_name)
# trait_value_cult <- as.vector(trait_cult$trait_value)
# 
# t_trait_cult <- data.frame(rbind(sp_cult, trait_name_cult, trait_value_cult))
# colnames(t_trait_cult) <- t_trait_cult[1,] 
# t_trait_cult <- t_trait_cult[-1,]

### filtering for growth forms trait

gf_all <- dplyr::filter(trait_all, trait_name %in% "whole plant growth form")
gf_cult <- dplyr::filter(trait_cult, trait_name %in% "whole plant growth form")

### fixing typing differences in trait values
gf_all$trait_value[gf_all$trait_value =="tree"] <- "Tree"
gf_cult$trait_value[gf_cult$trait_value =="tree"] <- "Tree"

gf_all$trait_value[gf_all$trait_value =="shrub"] <- "Shrub"
gf_cult$trait_value[gf_cult$trait_value =="shrub"] <- "Shrub"

gf_all$trait_value[gf_all$trait_value =="forb"] <- "Forb"
gf_cult$trait_value[gf_cult$trait_value =="forb"] <- "Forb"

gf_all$trait_value[gf_all$trait_value =="epiphyte"] <- "Epiphyte"
gf_cult$trait_value[gf_cult$trait_value =="epiphyte"] <- "Epiphyte"

gf_all$trait_value[gf_all$trait_value =="liana"] <- "Liana"
gf_all$trait_value[gf_all$trait_value == "creeper*"] <- "Creeper"

### plotting the results in a barplot

all_50 <- ggplot(data = gf_all, 
       mapping = aes(x = trait_value)) +
  ggtitle("Growth form for 50 most abundant species in general") +
  geom_bar(col="black",
           fill = "darkgreen") + 
  xlab("Trait Value") +
  ylab("Specie") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
all_50

cult_50 <- ggplot(data = gf_cult, 
       mapping = aes(x = trait_value)) +
  ggtitle("Growth form for 50 most abundant cultivated species") +
  geom_bar(col = "black",
           fill = "darkred") +
  xlab("Trait Value") +
  ylab("Specie") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
cult_50

### ok, it doesn't seen to have much difference between the growth form of the two categories
### let's try to plot the bar with the traits of all species in the two categories
abund_all <- read.csv("data/processed/abund_sp.csv")
abund_cult <- read.csv("data/processed/abund_cultivated.csv")

### creating a vector with the species names of each category so we can do the request
### to BIEN db
abund_all <- as.vector(abund_all$Specie)
abund_cult <- as.vector(abund_cult$Specie)

### now we are going to use BIEN package to download the traits of those species
library(BIEN)

trait_all_raw <- BIEN_trait_species(abund_all)
trait_cult_raw <- BIEN_trait_species(abund_cult)

### getting only the columns with species, trait name and trait value
trait_all_raw <- trait_all_raw[,1:3]
trait_cult_raw <- trait_cult_raw[,1:3]

### exploring trait names
unique(trait_all_raw$trait_name)

### filtering for growth forms trait
gf_all_raw <- dplyr::filter(trait_all_raw, trait_name %in% "whole plant growth form")
gf_cult_raw <- dplyr::filter(trait_cult_raw, trait_name %in% "whole plant growth form")

### exploring trait values
unique(gf_all_raw$trait_value)

### fixing the typing differences in trait values
gf_all_raw$trait_value[gf_all_raw$trait_value =="tree"] <- "Tree"
gf_all_raw$trait_value[gf_all_raw$trait_value =="herb"] <- "Herb"
gf_all_raw$trait_value[gf_all_raw$trait_value =="shrub"] <- "Shrub"
gf_all_raw$trait_value[gf_all_raw$trait_value =="liana"] <- "Liana"
gf_all_raw$trait_value[gf_all_raw$trait_value =="aquatic*"] <- "Aquatic"
gf_all_raw$trait_value[gf_all_raw$trait_value =="aquatic"] <- "Aquatic"
gf_all_raw$trait_value[gf_all_raw$trait_value =="forb"] <- "Forb"
gf_all_raw$trait_value[gf_all_raw$trait_value =="shrub*"] <- "Shrub"
gf_all_raw$trait_value[gf_all_raw$trait_value =="liana "] <- "Liana"
gf_all_raw$trait_value[gf_all_raw$trait_value =="liana*"] <- "Liana"
gf_all_raw$trait_value[gf_all_raw$trait_value =="liana"] <- "Liana"
gf_all_raw$trait_value[gf_all_raw$trait_value =="sedge*"] <- "sedge"
gf_all_raw$trait_value[gf_all_raw$trait_value =="cyperoid*"] <- "Cyperoid"
gf_all_raw$trait_value[gf_all_raw$trait_value =="cyperoid"] <- "Cyperoid"
gf_all_raw$trait_value[gf_all_raw$trait_value =="sub-shrub*"] <- "sub-shrub"
gf_all_raw$trait_value[gf_all_raw$trait_value =="tree*"] <- "Tree"
gf_all_raw$trait_value[gf_all_raw$trait_value =="creeper*"] <- "Creeper"
gf_all_raw$trait_value[gf_all_raw$trait_value =="creeper"] <- "Creeper"
gf_all_raw$trait_value[gf_all_raw$trait_value =="parasite*"] <- "Parasite"
gf_all_raw$trait_value[gf_all_raw$trait_value =="climber*"] <- "Climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="climber"] <- "Climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="epiphyte*"] <- "Epiphyte"
gf_all_raw$trait_value[gf_all_raw$trait_value =="epiphyte"] <- "Epiphyte"
gf_all_raw$trait_value[gf_all_raw$trait_value =="vine"] <- "Vine"
gf_all_raw$trait_value[gf_all_raw$trait_value =="woody climber*"] <- "Woody climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="woody climber"] <- "Woody climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="graminoid"] <- "Graminoid"
gf_all_raw$trait_value[gf_all_raw$trait_value =="per grass"] <- "Grass"
gf_all_raw$trait_value[gf_all_raw$trait_value =="grass"] <- "Grass"
gf_all_raw$trait_value[gf_all_raw$trait_value =="brophyte"] <- "Brophyte"
gf_all_raw$trait_value[gf_all_raw$trait_value =="succulent"] <- "Succulent"
gf_all_raw$trait_value[gf_all_raw$trait_value =="scrambler"] <- "Scrambler"
gf_all_raw$trait_value[gf_all_raw$trait_value =="moss"] <- "Moss"
### let's see if there's something missing
unique(gf_all_raw$trait_value)

### subseting and removing values that I don't find meaning
subset(gf_all_raw, trait_value == "2")
subset(gf_all_raw, trait_value == "4")
subset(gf_all_raw, trait_value == "Terrestrial_")
subset(gf_all_raw, trait_value == "Bryoid")
subset(gf_all_raw, trait_value == "mushroom")
rm <- c(6871, 6872, 6873, 776, 12901, 13583, 9173)

gf_all_raw <- gf_all_raw[- rm,]
unique(gf_all_raw$trait_value)

### it is still a bit hard to visualize, so let's cluster some forms
gf_all_raw$trait_value[gf_all_raw$trait_value =="aquatic moss"] <- "Aquatic"
gf_all_raw$trait_value[gf_all_raw$trait_value =="aquatic herb"] <- "Aquatic"
gf_all_raw$trait_value[gf_all_raw$trait_value =="terrestrial bryoid"] <- "Brophyte"
gf_all_raw$trait_value[gf_all_raw$trait_value =="climbing herb"] <- "Climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="climbing shrub"] <- "Climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="herbaceous climber"] <- "Climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="tendril climber"] <- "Climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="Woody climber"] <- "Climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="hook climber"] <- "Climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="twiner"] <- "Climber"
gf_all_raw$trait_value[gf_all_raw$trait_value =="Epiphytic_Herb"] <- "Epiphyte"
gf_all_raw$trait_value[gf_all_raw$trait_value =="Non-woody epiphyte"] <- "Epiphyte"
gf_all_raw$trait_value[gf_all_raw$trait_value =="large vine"] <- "Vine"
gf_all_raw$trait_value[gf_all_raw$trait_value =="small tree"] <- "Tree"
gf_all_raw$trait_value[gf_all_raw$trait_value =="woody herb"] <- "Herb"
gf_all_raw$trait_value[gf_all_raw$trait_value =="succulent stem rosette"] <- "Succulent"
gf_all_raw$trait_value[gf_all_raw$trait_value =="scrambler with branch tendrils"] <- "Scrambler"
gf_all_raw$trait_value[gf_all_raw$trait_value =="acrocarpic moss"] <- "Moss"
gf_all_raw$trait_value[gf_all_raw$trait_value =="sub-shrub"] <- "Sub-shrub"
gf_all_raw$trait_value[gf_all_raw$trait_value =="herbaceous subshrub"] <- "Sub-shrub"
gf_all_raw$trait_value[gf_all_raw$trait_value =="cactus"] <- "Cactus"
gf_all_raw$trait_value[gf_all_raw$trait_value =="fern"] <- "Fern"
gf_all_raw$trait_value[gf_all_raw$trait_value =="hemiepiphyte"] <- "Hemiepiphyte"
gf_all_raw$trait_value[gf_all_raw$trait_value =="marsh"] <- "Marsh"
gf_all_raw$trait_value[gf_all_raw$trait_value =="neophyte"] <- "Neophyte"
gf_all_raw$trait_value[gf_all_raw$trait_value =="sedge"] <- "Sedge"
gf_all_raw$trait_value[gf_all_raw$trait_value =="treelet"] <- "Treelet"
gf_all_raw$trait_value[gf_all_raw$trait_value =="Trailing_Plant"] <- "Trailing Plant"
gf_all_raw$trait_value[gf_all_raw$trait_value =="vascular cryptogram"] <- "Vascular cryptogram"

### let's do the same thing to the cultivated subset
### exploring trait values
unique(gf_cult_raw$trait_value)

### fixing typing differences
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="tree"] <- "Tree"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="herb"] <- "Herb"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="shrub"] <- "Shrub"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="liana"] <- "Liana"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="aquatic*"] <- "Aquatic"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="aquatic"] <- "Aquatic"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="forb"] <- "Forb"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="shrub*"] <- "Shrub"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="liana "] <- "Liana"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="liana*"] <- "Liana"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="liana"] <- "Liana"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="sedge*"] <- "Sedge"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="sedge"] <- "Sedge"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="cyperoid*"] <- "Cyperoid"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="cyperoid"] <- "Cyperoid"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="sub-shrub*"] <- "Sub-shrub"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="sub-shrub"] <- "Sub-shrub"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="tree*"] <- "Tree"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="creeper*"] <- "creeper"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="parasite*"] <- "Parasite"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="climber*"] <- "Climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="climber"] <- "Climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="epiphyte*"] <- "Epiphyte"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="epiphyte"] <- "Epiphyte"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="vine"] <- "Vine"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="woody climber*"] <- "woody climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="graminoid"] <- "Graminoid"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="per grass"] <- "Grass"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="grass"] <- "Grass"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="succulent"] <- "Succulent"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="scrambler"] <- "Scrambler"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="moss"] <- "Moss"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="cactus"] <- "Cactus"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="fern"] <- "Fern"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="hemiepiphyte"] <- "Hemiepiphyte"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="marsh"] <- "Marsh"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="neophyte"] <- "Neophyte"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="sedge"] <- "Sedge"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="treelet"] <- "Treelet"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="Trailing_Plant"] <- "Trailing Plant"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="brophyte"] <- "Brophyte"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="creeper"] <- "Creeper"

unique(gf_cult_raw$trait_value)

### subseting and removing values that I don't find meaning
subset(gf_cult_raw, trait_value == "2")
subset(gf_cult_raw, trait_value == "Terrestrial_")
subset(gf_cult_raw, trait_value == "mushroom")
rm <- c(6638, 6639, 6640, 12195, 8842)
gf_cult_raw <- gf_cult_raw[- rm,]

unique(gf_cult_raw$trait_value)

### it is still a bit hard to visualize, so let's cluster some forms
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="aquatic moss"] <- "Aquatic"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="aquatic herb"] <- "Aquatic"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="terrestrial bryoid"] <- "Brophyte"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="climbing herb"] <- "Climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="climbing shrub"] <- "Climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="herbaceous climber"] <- "Climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="tendril climber"] <- "Climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="woody climber"] <- "Climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="hook climber"] <- "Climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="twiner"] <- "Climber"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="Epiphytic_Herb"] <- "Epiphyte"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="Non-woody epiphyte"] <- "Epiphyte"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="large vine"] <- "Vine"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="small tree"] <- "Tree"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="woody herb"] <- "Herb"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="succulent stem rosette"] <- "Succulent"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="scrambler with branch tendrils"] <- "Scrambler"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="acrocarpic moss"] <- "Moss"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="sub-shrub"] <- "Sub-shrub"
gf_cult_raw$trait_value[gf_cult_raw$trait_value =="herbaceous subshrub"] <- "Sub-shrub"

### plotting the results
all <- ggplot(data = gf_all_raw, 
       mapping = aes(x = trait_value)) +
  ggtitle("Growth form for all species") +
  geom_bar(col="black",
           fill = "darkgreen") +
  xlab("Trait Value") +
  ylab("Specie") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
all

cult <- ggplot(data = gf_cult_raw, 
       mapping = aes(x = trait_value)) +
  ggtitle("Growth form for cultivated species") +
  xlab("Trait Value") +
  ylab("Specie")+
  geom_bar(col = "black",
           fill = "darkred") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
cult

### saving the plots
if (!dir.exists("figs/")) dir.create("figs/")

all_50
ggplot2::ggsave(filename = "figs/growth_form_50_all_ggplot.png",
                dpi = 300,
                units = "in",
                width = 6,
                height = 3.5)

cult_50
ggplot2::ggsave(filename = "figs/growth_form_50_cult_ggplot.png",
                dpi = 300,
                units = "in",
                width = 6,
                height = 3.5)

all
ggplot2::ggsave(filename = "figs/growth_form_all_ggplot.png",
                dpi = 300,
                units = "in",
                width = 8,
                height = 3.5)
cult
ggplot2::ggsave(filename = "figs/growth_form_cult_ggplot.png",
                dpi = 300,
                units = "in",
                width = 8,
                height = 3.5)

### saving db with traits from all species and cultivated with no subset of the
### 50 most abundant

write.csv(x = trait_all_raw,
          file = "data/processed/trait_all_nosubset.csv",
          row.names = F)
write.csv(x = trait_cult_raw,
          file = "data/processed/trait_cult_nosubset.csv",
          row.names = F)
