setwd("~/Documents/IlusaoControle")
library(tidyverse)



dados <- readr::read_csv("dados.csv")




dados %>% 
  pivot_longer(-id, names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_escolhas=escolhas/12, taxa_escolhas=replace_na(taxa_escolhas, 0))->escolhas


escolhas %>% 
  cbind(taxa_escolhas_norm=qnorm((rank(escolhas$escolhas, na.last = "keep") - 0.5) / sum(!is.na(escolhas$escolhas))))%>% 
  cbind(avaliação_norm=qnorm((rank(escolhas$avaliação, na.last = "keep") - 0.5) / sum(!is.na(escolhas$avaliação))))->escolhas




dados %>%  
  pivot_longer(-id, names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_sucesso=ganhos/escolhas, taxa_sucesso=replace_na(taxa_sucesso, 0),
                                     avaliação=replace_na(avaliação, 0))->sucesso

sucesso %>% 
  cbind(taxa_sucesso_norm=qnorm((rank(sucesso$taxa_sucesso, na.last = "keep") - 0.5) / sum(!is.na(sucesso$taxa_sucesso))))%>% 
  cbind(avaliação_norm=qnorm((rank(sucesso$avaliação, na.last = "keep") - 0.5) / sum(!is.na(sucesso$avaliação))))->sucesso




cor.test(escolhas$escolhas,escolhas$avaliação, method = "spearman")->a












dados %>% 
  pivot_longer(-c("id","Coleta"), names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  filter(!is.na(empresa)) %>% mutate(taxa_sucesso=ganhos/escolhas, taxa_sucesso=replace_na(taxa_sucesso, 0))%>% 
  cbind(taxa_escolhas_norm=qnorm((rank(escolhas$escolhas, na.last = "keep") - 0.5) / sum(!is.na(escolhas$escolhas))))%>% 
  cbind(avaliação_norm=qnorm((rank(escolhas$avaliação, na.last = "keep") - 0.5) / sum(!is.na(escolhas$avaliação)))) %>% 
  ggplot()+
  geom_point(aes(x=escolhas/12, y=avaliação,shape=empresa),position=position_dodge(width = 0.040), size=3)+
  geom_smooth(aes(x=escolhas/12, y=avaliação), method = "lm", se = F, col="black")+
  geom_text(data=data_frame(x=0.8, y=2),
            aes(x=x, y=y, label=paste0("ρ = ",round(a$estimate,2),
                                          ", p = ", 
                                       formatC(a$p.value, format="e", digits = 2 ))))+
  xlab("Taxa de Escolhas")+
  ylab("Avaliação")+
  scale_size(limits = c(2,4))+
  scale_shape_manual(values=1:2)


ext.functions::extplot( "Avaliação das empresas versus número de escolhas",3,0999,
                       theme=ggthemes::theme_clean(),w = 8, h=6)








cor.test(sucesso$taxa_sucesso,sucesso$avaliação, method = "spearman")->b


dados %>% 
  pivot_longer(-c("id","Coleta"), names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  cbind(taxa_sucesso_norm=sucesso$taxa_sucesso)%>% 
  cbind(avaliação_norm=(sucesso$avaliação)) %>% 
  ggplot()+
  geom_point(aes(x=taxa_sucesso_norm, y=avaliação_norm,shape=empresa),position=position_dodge(width = 0.040), size=3)+
  geom_smooth(aes(x=taxa_sucesso_norm, y=avaliação_norm), method = "lm", se = F, col="black")+
  geom_text(data=data_frame(x=0.8, y=2.0),
            aes(x=x, y=y, label=paste0("ρ = ",round(b$estimate,2),
                                       ", p = ", 
                                       formatC(b$p.value, format="e", digits = 2 ))))+
  xlab("Taxa de Sucesso")+
  ylab("Avaliação")+
  scale_size(limits = c(2,4))+
  scale_shape_manual(values=1:2)
  

ext.functions::extplot( "Avaliação das empresas versus taxa de sucesso",2,0999,
                        theme=ggthemes::theme_clean(),w = 8, h=6)





cor.test(escolhas$taxa_escolhas, sucesso$taxa_sucesso,method = "spearman")->c


dados %>% 
  pivot_longer(-c("id","Coleta"), names_to=c(".value", "empresa"), names_pattern="([^_]+)_(.*)") %>% 
  cbind(taxa_sucesso_norm=sucesso$taxa_sucesso)%>% 
  cbind(avaliação_norm=(escolhas$taxa_escolhas)) %>% 
  ggplot()+
  geom_point(aes(x=taxa_sucesso_norm, y=avaliação_norm,shape=empresa),position=position_dodge(width = 0.040), size=3)+
  geom_smooth(aes(x=taxa_sucesso_norm, y=avaliação_norm), method = "lm", se = F, col="black")+
  geom_text(data=data_frame(x=0.8, y=0.0),
            aes(x=x, y=y, label=paste0("ρ = ",round(c$estimate,2),
                                       ", p = ", 
                                       formatC(c$p.value, format="e", digits = 2 ))))+
  xlab("Taxa de Sucesso")+
  ylab("Taxa da Escolha")+
  scale_size(limits = c(2,4))+
  scale_shape_manual(values=1:2)


ext.functions::extplot( "Taxa de escolhas das empresas versus taxa de sucesso",1,0999,
                        theme=ggthemes::theme_clean(),w = 8, h=6)

