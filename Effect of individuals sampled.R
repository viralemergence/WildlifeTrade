library(tidyverse)

# Import all datasheets
s1 <- readxl::read_xls("mmc2.xls", sheet = 1)
s2 <- readxl::read_xls("mmc2.xls", sheet = 2)
s3 <- readxl::read_xls("mmc2.xls", sheet = 3)
s4 <- readxl::read_xls("mmc2.xls", sheet = 4)

# Sheet 1 contains traded status and viral summary information for mammal
# host species
# Check how many unique mammal hosts are in this datasheet
n_distinct(s1$`Species Name`)

# So records traded status for the 1,120 unique mammal species that were
# "screened for viruses" as indicated in Table 1
# What is the traded status of these 1,120 species?
table(s1$Category) 

# Matches with numbers reported in Table 1 for Domesticated, Traded, and 
# Nontraded species ("Traded" and "Future traded" are presented as just
# "traded" in Table 1
# But note that not all of these 1,120 species actually have viral data
# In other words, 1,120 species do not appear in the host-virus association
# datasheet
# Sheet 2 contains mammal host-virus associations

# Check how many unique mammal hosts are in this datasheet

n_distinct(s2$species) # Ostensibly 1,081 host species

# Problem: the paper never really says that 1,081 mammal host species is 
# actually the relevant number for their viral analyses?

# Check how many unique viruses are in this datasheet

n_distinct(s2$locality) # 1,690 virus species

# Problem: the paper reports that they have 1,682 viruses?

# Take sheet 1 species names and modify them to match with format of sheet 2
s1$host.name.mod <- str_replace_all(s1$`Species Name`, " ", "_")
# Not all sheet 2 species actually appear in sheet 1?
sum(unique(s2$species) %in% unique(s1$host.name.mod))
unique(s2$species)[which(!unique(s2$species) %in% unique(s1$host.name.mod))]
# Sheet 1 Dermanura cinereus = sheet 2 Artibeus cinereus
# Sheet 1 Sapajus apella becomes BOTH sheet 2 Sapajus apella AND sheet 2 Cebus apella
# "Rhinolophus affinis" and "Rhinopoma_hardwickii " in sheet 2 are
# typographic errors
# Join Category info onto sheet 2 host-virus associations
joined <- left_join(
  s2, 
  select(s1, host.name.mod, Category),
  by = c("species" = "host.name.mod")
)
# Manually fill in Category info that's missing because of 
# taxonomy/formatting issues
joined$Category[joined$species == "Artibeus_cinereus"] <- "Nontraded"
joined$Category[joined$species == "Cebus_apella"] <- "Traded"
# These two are tough to replace because grep() will not match with the 
# strange whitespace characters in them
joined$Category[grepl("Rhinolophus", joined$species) & (joined$locality == "Longquan") & (is.na(joined$Category))] <- "Future traded"
joined$Category[grepl("Rhinopoma", joined$species) & (joined$locality == "MERS") & (is.na(joined$Category))] <- "Nontraded"
joined.distinct <- distinct(joined, species, Category) %>%
  arrange(species)
nrow(joined.distinct)
table(joined.distinct$Category, useNA = "ifany")
# 36 Domestic, 62 Future traded, 561 Nontraded, 422 Traded

# Cleaning host names

# First of all inconsistent formatting in sheet 2 generates extra species that are not really species: "Rhinolophus affinis" vs. "Rhinolophus_affinis"
joined.distinct$species[891]
# This is a weird whitespace character thing that I assume they missed in cleaning
# Fix this
joined.distinct$species[891] <- "Rhinolophus_affinis"
# Also have both "Rhinopoma_hardwickii" and "Rhinopoma_hardwickii " (with 
# weird trailing space that is hard to remove)
joined.distinct$species[923]
# Fix this
joined.distinct$species[923] <- "Rhinopoma_hardwickii"
# Redo the distinct function
joined.distinct <- distinct(joined.distinct, species, Category)
n_distinct(joined.distinct$species)

# So really they've got 1,079 mammal species with viral info
table(joined.distinct$Category, useNA = "ifany")
# 36 Domestic, 61 Future traded, 560 Nontraded, 422 Traded
# Sheet 4

# Correlation between # of individuals sampled and viral richness
# Only available for 301 mammal species
plot(s4$`# of Individuals`, s4$`Virus richness`)
cor(s4$`# of Individuals`, s4$`Virus richness`)
# Stronger correlation than they report in the paper?
# In paper, they say the R^2 is 0.6?
# GLM shows this is definitely a significant effect
m <- lm(`Virus richness` ~ `# of Individuals`, data = s4)
summary(m)
