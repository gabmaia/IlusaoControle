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


cor.test(escolhas$taxa_escolhas,escolhas$avaliação, method = "pearson")->a


dados %>% 
  filter(id!="P48") %>% 
  pivot_longer(-c("id","Coleta"), names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_sucesso=ganhos/escolhas, taxa_sucesso=replace_na(taxa_sucesso, 0))%>% 
  ggplot()+
  geom_point(aes(x=escolhas, y=avaliação,shape=empresa),position=position_dodge(width = 0.20))+
  geom_smooth(aes(x=escolhas, y=avaliação), method = "lm", se = F, col="black")+
  geom_text(data=data_frame(x=9, y=1),
            aes(x=x, y=y, label=paste0("R = ",round(a$estimate,2),", p = ", 
                                       formatC(a$p.value, format="e", digits = 2 ))))


ext.functions::extplot( "Avaliação das empresas versus número de escolhas",1,0999,
                       theme=ggthemes::theme_clean())








cor.test(sucesso$taxa_sucesso,sucesso$avaliação, method = "pearson")->b


dados %>% 
  filter(id!="P48") %>% 
  pivot_longer(-c("id","Coleta"), names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_sucesso=ganhos/escolhas, taxa_sucesso=replace_na(taxa_sucesso, 0))%>% 
  ggplot()+
  geom_point(aes(x=taxa_sucesso, y=avaliação,shape=empresa),position=position_dodge(width = 0.20))+
  geom_smooth(aes(x=taxa_sucesso, y=avaliação), method = "lm", se = F, col="black")+
  geom_text(data=data_frame(x=0.7, y=1.8),
            aes(x=x, y=y, label=paste0("R = ",round(b$estimate,2),", p = ", 
                                       formatC(b$p.value, format="e", digits = 2 ))))

ext.functions::extplot( "Avaliação das empresas versus taxa de sucesso",2,0999,
                        theme=ggthemes::theme_clean())

