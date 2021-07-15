
library(ggridges)
library(magrittr)
library(tidyverse)
library(vroom)

trade <- read_csv("C:/Users/cjcar/Desktop/TradeWar.csv")

vir <- vroom("~/Github/virion/Virion/virion.csv.gz")

trade %<>% mutate(HighlyTraded = replace_na(`Highly traded`, 'N'))
trade %<>% rename(Host = `Species Name`) 
trade %<>% select(Host, Category)
trade %<>% mutate(Host = str_to_lower(Host))

vir %<>% filter(ICTVRatified==TRUE,
               HostNCBIResolved==TRUE,
               VirusNCBIResolved==TRUE,
               HostClass=="mammalia",
               !is.na(Host),
               !is.na(Virus))

vir %>% filter(Host == "homo sapiens") %>%
  select(Virus) %>% distinct() %>% pull(Virus) -> zoonoses

vir %>% 
  select(Host, Virus) %>%
  distinct() %>%
  group_by(Host) %>%
  summarize(ZooRes = max(Virus %in% zoonoses),
            VirDiv = n_distinct(Virus),
            ZooDiv = sum(Virus %in% zoonoses),
            PropZoo = ZooDiv/VirDiv) %>%
  left_join(trade) %>%
  mutate(Category = replace_na(Category, "PresumedNontraded")) -> df

# cites

cites <- read_csv("~/Github/WildlifeTrade/Citation.csv")
cites %<>% mutate(Host = str_to_lower(Host))

df %<>% left_join(cites)  

# Simple path analysis

library(lavaan)
library(semPlot)

# add host info back in

vir %>% select(Host, HostOrder) %>% distinct() -> order
df %<>% left_join(order)

logdf <- df  %>% mutate(Citations = log(Citations + 1),
                       VirDiv = log(VirDiv + 1),
                       ZooDiv = log(ZooDiv + 1))
logdf %<>% filter(!(Category == "PresumedNontraded")) %>% # This step also inherently removes humans
            mutate(Domestic = as.numeric(Category == 'Domestic'),
                 Traded = as.numeric(Category == 'Traded'))

# How does the nonlinearity affect this?

library(ggpubr)

theme2 <- theme_pubr()
theme2$legend.position <- c(0.2, 0.8)

logdf %>% 
  mutate(Traded = factor(Traded)) %>% 
  mutate(Traded = recode(Traded, !!!c("1" = "Traded", "0" = "Non-traded"))) %>%
  rename(" " = "Traded") %>% 
  ggscatterhist(
    x = "Citations", y = "ZooDiv", color = " ",
    size = 2, alpha = 0.2,
    palette = c("#00AFBB", "#FC4E07"),
    margin.plot = "boxplot",
    margin.params = list(fill = " ", color = "black", size = 0.2),
    ylab = "Number of zoonotic viruses"
  ) -> g 

# Simplest model

model <-'
Citations ~ Traded + Domestic
VirDiv ~ Citations + Traded + Domestic
ZooDiv ~ Citations + Traded + Domestic 
Traded ~~ Domestic
ZooDiv ~~ VirDiv
'

fit <- cfa(model, data = logdf)
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit, 'std', layout = 'circle2', curvePivot = TRUE, exoCov = FALSE, residuals = FALSE, 
         nCharNodes = 0, label.cex = 1.3, label.norm = FALSE, label.scale = FALSE, 
         node.width = 2.5, node.height = 2.5, edge.label.cex = 1.1, fade = FALSE)

# What about proportion zoonotic

model <-'
Citations ~ Traded + Domestic
VirDiv ~ Citations + Traded + Domestic 
PropZoo ~ Citations + Traded + Domestic + VirDiv
Traded ~~ Domestic
'

fit <- cfa(model, data = logdf)
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit, 'std', layout = 'circle2', curvePivot = TRUE, exoCov = FALSE, residuals = FALSE, 
         nCharNodes = 0, label.cex = 1.3, label.norm = FALSE, label.scale = FALSE, 
         node.width = 2.5, node.height = 2.5, edge.label.cex = 1.1, fade = FALSE)


