

setwd("~/Documents/IlusaoControle")
library(tidyverse)



dados <- readr::read_csv("dados.csv")




dados %>% 
  filter(id!="P48") %>% 
  pivot_longer(-id, names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_escolhas=escolhas/12, taxa_escolhas=replace_na(taxa_escolhas, 0))->escolhas

dados %>%  
  filter(id!="P48") %>% 
  pivot_longer(-id, names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_sucesso=ganhos/escolhas, taxa_sucesso=replace_na(taxa_sucesso, 0),
                                     avaliação=replace_na(avaliação, 0))->sucesso



#Shapiro Wilk

sucesso$taxa_sucesso %>% shapiro.test()
escolhas$taxa_escolhas %>% shapiro.test()


qqPlot(escolhas$taxa_escolhas)

ggdensity(escolhas$taxa_escolhas)
