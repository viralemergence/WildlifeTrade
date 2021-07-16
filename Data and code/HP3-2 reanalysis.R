
library(magrittr)
library(tidyverse)

hp32 <- read_csv("TradeEdgelist.csv")

hp32 %<>% rename(Host = species, Virus = locality)

hp3 <- read_csv("~/Github/HP3/data/associations.csv")

hp3 %<>% select(hHostNameFinal,vVirusNameCorrected) %>%
  rename(Host = hHostNameFinal,
         Virus = vVirusNameCorrected)

traderaw <- read_csv("TradeMaster.csv")
trade <- traderaw

trade %<>% filter(!`Species Name`=="Mus_minutoides")

####### Some statistics we report in our analysis
# traded species (mean: 9.34 viruses, 2.28 zoonotic; median: 2 viruses, 1 zoonotic)
# non-traded species (mean: 6.29 viruses, 1.57 zoonotic; median: 2 viruses, 1 zoonotic)
# domesticated species (mean: 18.6 viruses, 10.4 zoonotic; median: 9 viruses, 4 zoonotic).
trade %>% select(Category, `Total virus richness`, `Zoono virus richness`) %>%
  group_by(Category) %>%
  summarize(Median = median(`Total virus richness`),
            Mean = mean(`Total virus richness`),
            MedianZoo = median(`Zoono virus richness`),
            MeanZoo = mean(`Zoono virus richness`))
####### 

trade %<>%  
  select(`Species Name`, Category) %>% 
  rename(Host = `Species Name`) %>% 
  mutate(Host = str_replace_all(Host, " ", "_"))

# Correct a handful of names for a proper merge

dictionary = c("Artibeus_cinereus" = "Dermanura_cinereus",
               "Bos_taurus_indicus" = "Bos_taurus",
               "Cebus_apella" = "Sapajus_apella")
hp32 %<>% mutate(Host = recode(Host, !!!dictionary))

# These two have unicode issues

hp32$Host[str_detect(hp32$Host, "affinis")] <- "Rhinolophus_affinis"
hp32$Host[str_detect(hp32$Host, "hardwickii")] <- "Rhinopoma_hardwickii"

# Merge the datasets

hp32 %<>% left_join(trade)

# All species integrate correctly 

hp32 %>% filter(is.na(Category)) %>% select(Host) %>% distinct()

# There's still some known data issues (e.g. Canis lupus) but let's proceed anyway 

################### QUESTIONS ABOUT CROSS-SPREADSHEET AGREEMENT

hp32 %>% select(Host, Virus) %>% distinct() %>% group_by(Host) %>% count() %>%
  rename(VirusRaw = n) %>% left_join(
traderaw %>% 
  filter(!`Species Name`=="Mus_minutoides") %>%
  select(`Species Name`, `Total virus richness`) %>%
  rename(Host = `Species Name`,
         VirusSummary = `Total virus richness`) %>%
  mutate(Host = str_replace_all(Host, " ", "_"))
) -> comparison

comparison %>% ggplot(aes(x = VirusRaw, y = VirusSummary)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)

comparison %<>% mutate(Difference = VirusSummary - VirusRaw)

# only 903 of 1077 matched species have the same reported total number of viruses; 
table(comparison$Difference==0)

# in all cases, the summarized viral richness is higher
table(comparison$Difference>0)

comparison %>% filter(Difference > 0) %>% arrange(-Difference)

comparison %>% filter(Difference > 0) %>% pull(Host) -> morehosts

# Although traded species are not disproportionately common in that sample, 
trade %>% filter(Host %in% morehosts) %>% count(Category)

# Traded species had higher observed differences
comparison %>% filter(Host %in% morehosts) %>% left_join(trade) %>%
  ggplot(aes(x = Category, y = Difference)) + geom_boxplot() + 
  scale_y_continuous(trans='log10')

# on average, traded species have a higher resulting inflation of reported viral richness (mean: +30 virus spp.) than nontraded (+19 spp.) and domestic species (+6 spp.).
comparison %>% filter(Host %in% morehosts) %>% left_join(trade) -> inflation.df
inflation.df %>% group_by(Category) %>% summarize(Difference = mean(Difference))

# What about zoonoses

virus <- read_csv("TradeVirusMetadata.csv")

hp32 %<>% mutate(Virus = str_to_lower(str_replace_all(Virus, " ", "_"))) %>%
  left_join(virus %>%
            select(`Virus name`, `Zoonotic status`) %>% 
            rename(Virus = "Virus name",
            Zoonotic = "Zoonotic status") %>%
              mutate(Virus = str_to_lower(str_replace_all(Virus, " ", "_"))) %>% 
              mutate(Virus = recode(Virus, !!!c("corona_virus" = "corona"))))

hp32 %>% filter(is.na(Zoonotic)) %>% pull(Virus) %>% unique()

hp32 %>% filter(Zoonotic == 1) %>% group_by(Host, Category) %>%
  summarize(ZoonoticRaw = n_distinct(Virus)) %>%
  left_join(traderaw %>%
              select(`Species Name`, `Zoono virus richness`) %>%
              rename(Host = `Species Name`,
                     ZoonoticSummary = `Zoono virus richness`) %>%
              mutate(Host = str_replace_all(Host, " ", "_"))
              ) -> comparison.z


####### Interlude: Some statistics we report in our analysis

# The study and virus metadata report 226 known zoonoses; however, six viruses are present in the virus metadata but absent from the edgelist: 
# two are zoonotic (Bangui virus and Lake Victoria marburgvirus, a redundancy with "Marburg Virus (MARV)") and four are non-zoonotic 
# (Gossas virus, Kolente virus, Menekre virus, and Ntaya virus). These absences may also be connected to differences in the 75% statistic.	
virus[!(str_to_lower(str_replace_all(virus$`Virus name`, " ", "_")) %in% hp32$Virus),]

#  traded species actually account for 39% (421 of 1,076) of all hosts in the sample
hp32 %>% select(Host, Category) %>% distinct() %>% count(Category)

# only appear to host 68% (153 of 224) of zoonotic viruses
hp32 %>% filter(Zoonotic == 1) %>% pull(Virus) %>% unique() %>% length()
hp32 %>% filter(Zoonotic == 1, Category == "Traded") %>% pull(Virus) %>% unique() %>% length()
153/224 

# Including the "future traded" species - predictions of which species might be involved in future wildlife trade - raises this to 158 of 224 (70.5%).
hp32 %>% filter(Zoonotic == 1, Category %in% c("Traded", "Future traded")) %>% pull(Virus) %>% unique() %>% length()
158/224
  
####### 

comparison.z %>% ggplot(aes(x = ZoonoticRaw, y = ZoonoticSummary)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)

comparison.z %<>% mutate(Difference = ZoonoticSummary - ZoonoticRaw)

table(comparison.z$Difference)
table(sign(comparison.z$Difference))
comparison.z %>% filter(!(Difference == 0)) %>% arrange(-Difference)

#  traded species tend to gain zoonotic viruses (+1: 15 spp.; +2: 1 sp.; +3: 1 sp.; -1: 1 sp.), and nontraded species tend to lose them (-1: 7 spp.; +1: 1 sp.).
table(comparison.z$Category, comparison.z$Difference)

################### OTHER DESCRIPTIVE WITH THE CLEAN DATA

# in the association data provided with the study, 422 species are traded out of 1,081 total mammal species (39.0%). 
# 3.3% (35 of 1,076) of species in the dataset are domesticated

hp32 %>% select(Host) %>% distinct() %>% nrow()
hp32 %>% select(Host, Category) %>% distinct() %>% count(Category)

hp32 %>%
  group_by(Host, Category) %>%
  summarize(VirDiv = n_distinct(Virus)) -> virdf 
hp32 %>%
  filter(Zoonotic==1) %>% 
  group_by(Host, Category) %>%
  summarize(ZooDiv = n_distinct(Virus)) -> zoodf
mediandf <- left_join(virdf, zoodf) %>% 
  mutate(ZooDiv = replace_na(ZooDiv, 0))

# a marginal difference between traded species (mean: 5.30 viruses, 2.19 zoonotic; median: 2 viruses, 1 zoonotic) 
# and non-traded species (mean: 3.42 viruses, 1.55 zoonotic; median: 2 viruses, 1 zoonotic), 
# particularly compared to domesticated species (mean: 18.6 viruses, 10.9 zoonotic; median: 9 viruses, 5 zoonotic).

mediandf %>% 
  group_by(Category) %>%
  summarize(Median = median(VirDiv),
            Mean = mean(VirDiv),
            MedianZoo = median(ZooDiv),
            MeanZoo = mean(ZooDiv))

################### QUESTIONS ABOUT THE NEW DATA

new <- setdiff(hp32 %>% select(Host, Virus), hp3 %>% mutate(Virus = str_to_lower(str_replace_all(Virus, " ", "_"))))

new %<>% left_join(trade)

new %>% count(Category)

new %>% select(Host, Category) %>% distinct() %>% count(Category)

####### Some statistics we report in our analysis

# 36% of the original data is PREDICT virus associations
nrow(hp32 %>% filter(str_detect(Virus, "predict_")))/nrow(hp32)

# 17% of the associations come from non-PREDICT literature review
nrow(new %>% filter(!str_detect(Virus, "predict_")))/nrow(hp32)

# This manually-compiled component is oversampled for hosts involved in the wildlife trade (Traded: 291; Nontraded: 241)
new %>% filter(!str_detect(Virus, "predict")) %>% select(Host, Category) %>% distinct() %>% count(Category) 

# and their associations with viruses (Traded: 758; Nontraded: 485), 
new %>% filter(!str_detect(Virus, "predict")) %>% count(Category)

# PREDICT does not even show a comparable level of oversampling in hosts (Traded: 67; Nontraded: 152) 
new %>% filter(str_detect(Virus, "predict_")) %>% select(Host, Category) %>% distinct() %>% count(Category) 

# and associations (Traded: 478; Nontraded: 510)
new %>% filter(str_detect(Virus, "predict_")) %>% count(Category)
#######
