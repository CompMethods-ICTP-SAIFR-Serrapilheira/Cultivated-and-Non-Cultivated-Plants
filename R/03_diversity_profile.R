### diversity profiles of each category
library(vegan)
library(dplyr)

### we are going to use the abundance data frames
total_abund_50 <- read.csv("data/processed/total_abund_50.csv")

### we are going to compare some indexes of diversity, considering the subset of 
### the 50 most abundant species in general and the 50 most abundant cultivated species
### to do so, we are going to transform their abundance matrix into a vector

all_sp <- as.vector(total_abund_50$freq..all)
cult_sp <- as.vector(total_abund_50$freq..cultivated)

### using Shannon diversity function
sh_all <- diversity(all_sp, index = "shannon")
sh_cult <- diversity(cult_sp, index = "shannon")
# that values were almost the same

### let's do the inverted Simpson diversity index for both
invsimp_all <- diversity(all_sp, index = "invsimpson")
invsimp_cult <- diversity(cult_sp, index = "invsimpson")
# it doesn't seem to have much difference too

### now, let's apply the Rényi function
### according to the theory of diversity ordering, one community can be regarded 
### as more diverse than another only if its Rényi diversities are all higher 
renyi_all <- renyi(all_sp)
renyi_cult <- renyi(cult_sp)

plot(renyi_all)
plot(renyi_cult)

### we are going to bind the two vectors for Renyi diversity so we can plot then together
renyi_bind <- cbind(renyi_all, renyi_cult)
renyi_bind <- as.data.frame(cbind(renyi_all, renyi_cult)) 


plot(renyi_bind)

matplot(renyi_bind, type = "l", axes = F, main = "Renyi Diversity: most abundant species") 
box()
axis(side = 2)
axis(side = 1)
legend("bottomleft",
       legend = c("All species", "Cultivated species"),
       lty = c(1,2),
       col = c(1,2))
### as they cross each other, depending on the scale of your Renyi you can say 
### that the communities are not that different in terms of diversity.

### but what if we do the same with the whole set of species, not only the 50 most abundant?
### let's load the data
abund_all <- read.csv("data/processed/abund_sp.csv")
abund_cultivated <- read.csv("data/processed/abund_cultivated.csv")

### turning NAs into zeros
abund_all[is.na(abund_all)] <- 0
abund_cultivated[is.na(abund_cultivated)] <- 0

### creating vectors with the abundance numbers
abund_all_vec <- as.vector(abund_all$Freq)
abund_cult_vec <- as.vector(abund_cultivated$Freq)

### Shannon diversity
all_sp_sh <- diversity(abund_all_vec, index = "shannon")
cult_sp_Sh <- diversity(abund_cult_vec, index = "shannon")
### again, not much difference in the resulting numbers

### let's do the inverted Simpson diversity index for both
invsimp_all_sp <- diversity(abund_all_vec, index = "invsimpson")
invsimp_cult_sp <- diversity(abund_cult_vec, index = "invsimpson")
### now it seems to have more difference
# t test?

### now, let's apply the Rényi function
### according to the theory of diversity ordering, one community can be regarded 
### as more diverse than another only if its Rényi diversities are all higher 
renyi_all_sp <- renyi(abund_all_vec)
renyi_cult_sp <- renyi(abund_cult_vec)

plot(renyi_all_sp)
plot(renyi_cult_sp)

renyi_bind_sp <- cbind(renyi_all_sp, renyi_cult_sp)

plot(renyi_bind_sp)

matplot(renyi_bind_sp, type = "l", axes = F, main = "Renyi Diversity") 
box()
axis(side = 2)
axis(side = 1)
legend("topright",
       legend = c("All species", "Cultivated species"),
       lty = c(1,2),
       col = c(1,2))

### now the curves don't cross, so all the values of "all species" subset is higher
### than the values of "cultivated species" subset, which means that the "all species"
### subset is more diverse

### WARNING, the plots were exported by hand!!!
