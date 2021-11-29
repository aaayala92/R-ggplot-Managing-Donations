#Library
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(forcats)
library(readxl)
coromoto <- read_excel("C:/Users/Carrasquero & Sucre/Desktop/R projects/Coromoto2020/coromoto.xlsx", 
                       col_types = c("date", "numeric", "text", 
                                     "text", "text", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "text"))

str(coromoto)

coromoto$Centro_Acopio<-as.factor(coromoto$Centro_Acopio)
tail(coromoto)

levels(coromoto$Centro_Acopio)
summary(coromoto$Centro_Acopio)

pal<-colorRampPalette(c("#6E233B", "#ffffff"))(13) 

#Gráfico por Colegio

coromoto %>% ggplot(aes(fct_infreq(Centro_Acopio),fill=fct_infreq(Centro_Acopio)))+
  geom_bar(fill="#6E233B")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="", y= "Donaciones", title= "Donantes por colegio | Acumulado",fill="Centro de Acopio")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#ffffff"))+
  ylim(0,100)+
  theme(axis.text=element_text(size=9),axis.text.x = element_text(angle = 45, hjust = 1))

  


table(coromoto$Centro_Acopio)
nrow(coromoto)


coromoto2<-gather(coromoto,"Donacion","Cantidad",8:22)
coromoto2<-subset(coromoto2,select=-c(Voluntario_2,Correo,Otros_Donativos))

str(coromoto2)
coromoto2[is.na(coromoto2)]<-0


coromoto2 %>% group_by(Donacion) %>%  summarise(tot=sum(Cantidad)) %>% arrange(desc(tot)) %>% 
  mutate(Pareto=tot/sum(tot))

str(coromoto)



coromoto2 %>% summarise(tot=sum(Cantidad))



#Grafico por tipo de donación

coromoto2 %>% group_by(Donacion) %>% mutate(tot=sum(Cantidad)) %>% 
  ggplot(aes(reorder(Donacion,Cantidad,sum),Cantidad,fill=Centro_Acopio))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=pal)+
  labs(x="Donaciones", y= "Cantidad", title= "Total de Donaciones | Acumulado",fill="Centro de Acopio")+
  geom_hline(yintercept=0,color="black",size=1)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#ffffff"))+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Donacion), geom = "text",hjust = -0.5)




coromoto2 %>% group_by(Donacion) %>% 
  ggplot(aes(reorder(Donacion,Cantidad,sum),Cantidad,fill=Centro_Acopio))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=pal)+
  labs(x="Donaciones", y= "Cantidad", title= "Donaciones | Acumulado",fill="Centro de Acopio")+
  geom_hline(yintercept=0,color="black",size=1)+
  facet_grid(.~Centro_Acopio )+
  theme_bw()+
  theme(legend.position = "none", panel.background = element_rect(fill = "#ffffff"),strip.text.x = element_text(size = 7))+
  stat_summary(data=subset(coromoto2,Cantidad != 0),fun.y = sum, aes(label = ..y.., group = Donacion), geom = "text",hjust = -0.2,size=3)



#HOY


coromoto2day<-coromoto %>% filter(Fecha > "2020-07-10")

pal2<-colorRampPalette(c("#CCA43B", "#222222"))(13) 

#Gráfico por Colegio

coromoto2day %>% ggplot(aes(fct_infreq(Centro_Acopio),fill=fct_infreq(Centro_Acopio)))+
  geom_bar(fill="#CCA43B")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Centro de Acopio", y= "Donaciones", title= "Donantes por colegio del día 15/07/2020",fill="Centro de Acopio")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#ffffff"))+
  ylim(0,25)+
  theme(axis.text=element_text(size=9),axis.text.x = element_text(angle = 45, hjust = 1))



table(coromoto2day$Centro_Acopio)
nrow(coromoto2day)


coromoto2day2<-gather(coromoto2day,"Donacion","Cantidad",8:22)
coromoto2day2<-subset(coromoto2day2,select=-c(Voluntario_2,Correo,Otros_Donativos))
coromoto2day2[is.na(coromoto2day2)]<-0


coromoto2day2 %>% group_by(Donacion) %>%  summarise(tot=sum(Cantidad)) %>% arrange(desc(tot)) %>% 
  mutate(Pareto=tot/sum(tot))

coromoto2day2 %>% summarise(tot=sum(Cantidad))



#Grafico por tipo de donación

coromoto2day2 %>% group_by(Donacion) %>% mutate(tot=sum(Cantidad)) %>% 
  ggplot(aes(reorder(Donacion,Cantidad,sum),Cantidad,fill=Centro_Acopio))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=pal2)+
  labs(x="Donaciones", y= "Cantidad", title= "Total de Donaciones del día 15/07/2020",fill="Centro de Acopio")+
  geom_hline(yintercept=0,color="black",size=1)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#ffffff"))+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Donacion), geom = "text",hjust = -0.4)


coromoto2day2 %>% group_by(Donacion) %>% 
  ggplot(aes(reorder(Donacion,Cantidad,sum),Cantidad,fill=Centro_Acopio))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=pal2)+
  labs(x="Donaciones", y= "Cantidad", title= "Donaciones del día 15/07/2020",fill="Centro de Acopio")+
  geom_hline(yintercept=0,color="black",size=1)+
  facet_grid(.~Centro_Acopio )+
  theme_bw()+
  theme(legend.position = "none", panel.background = element_rect(fill = "#ffffff"),strip.text.x = element_text(size = 7))+
  stat_summary(data=subset(coromoto2day2,Cantidad != 0),fun.y = sum, aes(label = ..y.., group = Donacion), geom = "text",hjust = -0.4,size=3)


#Extra

coromoto3<-gather(coromoto,"Donacion","Cantidad",8:22)
str(coromoto3)


coromoto2 %>%  group_by(Centro_Acopio,Benefactor) %>%
  summarise(tot=sum(Cantidad)) %>% 
  ggplot(aes(reorder(Centro_Acopio,tot,mean),tot,fill=Centro_Acopio))+
  geom_boxplot(na.rm = TRUE)+
  geom_jitter(alpha=0.5, na.rm = TRUE,width=0.15)+
  geom_hline(yintercept = 6, size=1,color="#CCA43B")+
  geom_text(aes(11, 6,label = paste("Mediana de Unidades por Donación =", 6),
                vjust = -0.3),size=3.5)+
  scale_y_log10(breaks=c(1,3,5,10,25,50,100,200),labels=c(1,3,5,10,25,50,100,200))+
  scale_fill_manual(values=pal)+
  labs(x="", y= "Cantidad (Escala log)", title= "Cantidades recibidas por Donación | Acumulado",fill="Centro de Acopio")+
  theme_bw()+
theme(legend.position = "none", panel.background = element_rect(fill = "#ffffff"),strip.text.x = element_text(size = 7),
      axis.text.x = element_text(angle = 45, hjust = 1))


  a<-coromoto2 %>%  group_by(Centro_Acopio,Benefactor) %>%
    summarise(tot=sum(Cantidad))
median(a$tot)
mean(a$tot)

coromoto2 %>% group_by(Fecha) %>% summarise(tot=sum(Cantidad)) %>% 
  ggplot(aes(Fecha,tot))+ 
  geom_col(fill="#6E233B")+
  geom_hline(yintercept = 0, size=1,color="#CCA43B")+
  labs(x="", y= "Total de Unidades", title= "Total de Unidades por Jornada de Recolección")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "#ffffff"))+
  theme(axis.text=element_text(size=9),axis.text.x = element_text(angle = 45, hjust = 1))


