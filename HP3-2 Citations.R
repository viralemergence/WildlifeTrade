
# Taken from https://github.com/viralemergence/virionette/blob/master/04_predictors/Citations.R

# Takes an hour or two ####

library(easyPubMed)
library(magrittr)
library(tidyverse)

trade <- read_csv("TradeMaster.csv")

CiteCounter <- function(Name) {
  Name %>% str_trim %>% 
    get_pubmed_ids %>% 
    extract2("Count") %>% 
    as.character %>% 
    as.numeric
}

trade %<>% select(`Species Name`) %>%
  rename(Host = `Species Name`)

trade$Citations = NA

i <- 1

for(i in i:nrow(trade)){
  
  Sp <- trade$Host[i]
  
  print(Sp)
  
  Sp <- paste(paste("\"", Sp, sep=''), "\"", sep='')
  
  trade$Citations[i] <- CiteCounter(Sp)
  
  print(paste0("Citations: ", trade$Citations[i]))
  
}

write_csv(trade, "Citation.csv")
