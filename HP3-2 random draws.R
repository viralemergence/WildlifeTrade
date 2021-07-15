

library(magrittr)
library(tidyverse)

set.seed(1234)

source("HP3-2 reanalysis.R")

hp32 %<>% select(-Category) %>% distinct()

hosts <- hp32 %>% pull(Host) %>% unique()

n.host <- hosts %>% length()

n.zoon <- hp32 %>% filter(Zoonotic == 1) %>% pull(Virus) %>% unique() %>% length()
  
# Wildlife trade

n.sample <- floor(n.host*0.39)

s <- sapply(c(1:1000), function(i) {
  print(i)
  host.i <- sample(hosts, n.sample)
  p.zoon <- hp32 %>% filter(Host %in% host.i, Zoonotic == 1) %>% pull(Virus) %>% unique() %>% length() / n.zoon
})

mean(s)
sd(s)

table(s > 0.75)

# Domestication

n.sample <- floor(n.host*0.033)

s <- sapply(c(1:1000), function(i) {
  print(i)
  host.i <- sample(hosts, n.sample)
  p.zoon <- hp32 %>% filter(Host %in% host.i, Zoonotic == 1) %>% pull(Virus) %>% unique() %>% length() / n.zoon
})

mean(s)
sd(s)

table(s > 0.75)
