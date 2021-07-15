
library(magrittr)
library(tidyverse)

trade <- read_csv("TradeMaster.csv")

### Remove the two species with unavoidable data errors
trade %<>% filter(!(`Species Name` %in% c("Rattus leucopus", "Manis pentadactyla")))
###

### Let's reproduce some models

trade %<>% rename(Host = `Species Name`,
                  VirDiv = `Total virus richness`,
                  ZooDiv = `Zoono virus richness`) %>%
  mutate(Host = str_replace(Host, " ", "_")) %>%
  select(Host, Category, VirDiv, ZooDiv)

cites <- read_csv("~/Github/WildlifeTrade/Citation.csv")
cites %<>% mutate(Host = str_replace(Host, " ", "_"))

trade %<>% left_join(cites)

logdf <- trade %>% mutate(Citations = log(Citations + 1),
                          VirDiv = log(VirDiv + 1),
                          ZooDiv = log(ZooDiv + 1),
                          PropZoo = ZooDiv/VirDiv)

logdf %<>% mutate(Domestic = as.numeric(Category == 'Domestic'),
                  Traded = as.numeric(Category == 'Traded'))

# Simplest model

library(lavaan)
library(semPlot)

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

# PropZoo

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

# Fig 1D

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
    margin.params = list(fill = " ", color = "black", size = 0.2),
    ylab = "Number of zoonotic viruses"
  ) -> g 