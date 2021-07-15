
# Taken from https://github.com/viralemergence/virionette/blob/master/04_predictors/Citations.R

# Takes an hour or two ####

library(easyPubMed)
library(magrittr)
library(tidyverse)

trade <- read_csv("TradeMaster.csv")

CiteCounter <- function(Name) {
  Query <- Name %>% str_trim %>%
    str_replace_all("_", " ") %>% 
    paste0("\"", ., "\"") %>% 
    get_pubmed_ids
  Original <- Query$OriginalQuery
  Translation <- Query$QueryTranslation
  if(str_count(Original) == (str_count(Translation) - 12)){
    Query %>% 
      extract2("Count") %>% 
      as.character %>% 
      as.numeric %>% 
      return
  }else{
    print(paste0(Name, ": Maybe failure?"))
    return(0)
  }
}

trade %<>% select(`Species Name`) %>%
  rename(Host = `Species Name`)

trade$Citations = NA

i <- 1

for(i in i:nrow(trade)){
  
  Sp <- trade$Host[i]
  
  print(Sp)
  
  trade$Citations[i] <- CiteCounter(Sp)
  
  print(paste0("Citations: ", trade$Citations[i]))
  
}

write_csv(trade, "Citation.csv")
