setwd("~/Documents/IlusaoControle")
library(tidyverse)



dados <- readr::read_csv("dados.csv")

dados %>% 
  pivot_longer(-c("id","Coleta"), names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_sucesso=ganhos/escolhas, taxa_sucesso=replace_na(taxa_sucesso, 0))%>% 
  ggplot(aes(x=escolhas, y=avaliação,col=Coleta))+
  geom_point()



dados %>% 
  filter(id!="P24") %>% 
  pivot_longer(-id, names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_sucesso=ganhos/escolhas, taxa_sucesso=replace_na(taxa_sucesso, 0))->bob

chisq.test(bob$escolhas,bob$avaliação)

cor.test(bob$taxa_sucesso,bob$avaliação, method = "pearson")

library(ggpubr)

ggqqplot(bob$escolhas)

bob %>% ggscatter(x="taxa_sucesso", y="avaliação", add = "reg.line", conf.int = F, 
                  cor.coef = TRUE, cor.method = "pearson")






dados %>% 
  filter(id!="P24") %>% 
  pivot_longer(-id, names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_escolhas=escolhas/12, taxa_escolhas=replace_na(taxa_escolhas, 0))->bob2

chisq.test(bob2$escolhas,bob$avaliação)

cor.test(bob2$taxa_escolhas,bob$avaliação, method = "pearson")


ggqqplot(bob2$escolhas)

bob2 %>% ggscatter(x="taxa_escolhas", y="avaliação", add = "reg.line", conf.int = F, 
                  cor.coef = TRUE, cor.method = "pearson")