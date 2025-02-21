source("rdocs/source/packages.R")

library(readxl)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(patchwork)

Dados<-read_excel("Amostra_g09_FelipeBretas_Renan_Tales_VictorSouza.xlsx")
###Analise 1###########
####Gráfico do Local da escola
Dados[c(Dados[,4]==1),4]<-"Urbana"
Dados[c(Dados[,4]==2),4]<-"Rural"

Amostra50 <- Dados[c(198, 196, 190, 189, 186, 173, 171, 169, 167,
                                    154,147, 145, 144, 143, 141, 139, 132, 127,
                                    117, 116, 111 , 99, 98, 96, 95, 90, 88, 79,
                                    78, 77, 68,65,63, 60, 55, 52, 51, 48, 37, 35,
                                    33, 31, 21, 19, 17, 13, 12, 8, 7, 6),]

c_L <- Dados %>%
  group_by(LOCAL) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(LOCAL)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))

G1<-ggplot(c_L) +
  aes(x = factor(""), y = Prop , fill = factor(LOCAL)) +
  geom_bar(width = 1, stat = "identity") +coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") + scale_fill_brewer("Local", palette = "Blues")



#####Boxplot das notas
G2.1<-ggplot(Dados) +
  aes(
    x = factor(""),
    y = NOTA_LP
  ) +
  geom_boxplot(fill = c("blue"), width = 0.5) +
  guides(fill = none) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Nota") 
G2.2<-ggplot(Dados) +
  aes(
    x = factor(""),
    y = NOTA_MT
  ) +
  geom_boxplot(fill = c("lightblue"), width = 0.5) +
  guides(fill = none) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Nota")
G2<-G2.1+G2.2
#######
G3<-ggplot(Dados) +
  aes(x = REG) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "lightblue") +
  geom_text(aes(
    y = prop.table(..count..) * 100 + 0.5,
    label = paste0(gsub("\\.", ",", round(prop.table(..count..) * 100, 2)), "%")
  ),
  stat = "count", vjust = 0, size = 4
  ) +
  labs(x = "Região", y = "Frequência Relativa")

###Graf de tam mun
c_L2 <- Dados %>%
  group_by(TAM_MUN) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(TAM_MUN)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))

G4<-ggplot(c_L2) +
  aes(x = factor(""), y = Prop , fill = factor(TAM_MUN)) +
  geom_bar(width = 1, stat = "identity") +coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") + scale_fill_brewer("TAM_MUN", palette = "Blues")

##Graf ADM 

c_L3 <- Dados %>%
  group_by(ADM) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(ADM)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))

G5<-ggplot(c_L3) +
  aes(x = factor(""), y = Prop , fill = factor(ADM)) +
  geom_bar(width = 1, stat = "identity") +coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") + scale_fill_brewer("ADM", palette = "Blues")

##Graf tam esc

c_L4 <- Dados %>%
  group_by(TAM_ESCOLA) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(TAM_ESCOLA)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))

G6<-ggplot(c_L4) +
  aes(x = factor(""), y = Prop , fill = factor(TAM_ESCOLA)) +
  geom_bar(width = 1, stat = "identity") +coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") + scale_fill_brewer("TAM_ESCOLA", palette = "Blues")

##matriculados
G7<-ggplot(Dados) +
  aes(
    x = factor(""),
    y = MATRICULADOS
  ) +
  geom_boxplot(fill = c("lightblue"), width = 0.5) +
  guides(fill = none) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Matriculados") 

#####################analise 3#########
#alpha = 0,95
#Estimador de LP
miLP <- round((mean(Dados$NOTA_LP) - (1.96 * sd(Dados$NOTA_LP)/sqrt(200))), 2)
msLP <- round((mean(Dados$NOTA_LP) + (1.96 * sd(Dados$NOTA_LP)/sqrt(200))), 2)
#Amostra50
AiLP<- round((mean(Amostra50$NOTA_LP) - (2.0096 * sd(Amostra50$NOTA_LP)/sqrt(50))), 2)
AsLP<- round((mean(Amostra50$NOTA_LP) + (2.0096 * sd(Amostra50$NOTA_LP)/sqrt(50))), 2)

#Estimador de MT
miMT <- round((mean(Dados$NOTA_MT) - (1.96 * sd(Dados$NOTA_MT)/sqrt(200))), 2)
msMT <- round((mean(Dados$NOTA_MT) + (1.96 * sd(Dados$NOTA_MT)/sqrt(200))), 2)
#Amostra50
AiMT<-round((mean(Amostra50$NOTA_MT) - (2.0096 * sd(Amostra50$NOTA_MT)/sqrt(50))), 2)
AsMT<-round((mean(Amostra50$NOTA_MT) + (2.0096 * sd(Amostra50$NOTA_MT)/sqrt(50))), 2)

#########################Analise 4#########
((sd(Dados$NOTA_LP)/sqrt(200))*1.645)+(184.3)
mean(Dados$NOTA_LP)
((sd(Dados$NOTA_MT)/sqrt(200))*1.645)+(204.3)
mean(Dados$NOTA_MT)

#Amostra50
((sd(Amostra50$NOTA_LP)/sqrt(50))*1.6766)+(184.3)
mean(Amostra50$NOTA_LP)
((sd(Amostra50$NOTA_MT)/sqrt(50))*1.6766)+(204.3)
mean(Amostra50$NOTA_MT)
#############analise 8##################
D8<-Dados[Dados[,9]<75,]
c2 <- table(D8$REG, D8$LOCAL)%>%
  data.frame() %>%
  mutate(Pct = Freq / sum(Freq))

ordem1=c("NE","SE","N","S","CO")
ordem2=c("Urbana","Rural")

porcentagens <- str_c(round(c2$Pct*100, digits = 2), "%") %>% str_replace("
\\.", ",")

legendas <- str_squish(str_c(c2$Freq, " (", porcentagens, ")")
)

GA8<-ggplot(c2) +
  aes(
    x = factor(Var2,level=ordem2),
    y = Pct,
    fill = factor(Var1,level=ordem1), label=legendas
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(name = "Local") +
  scale_y_continuous(
    limits = c(0, .35), expand = c(0, 0), breaks = seq(0, .3, .05),
    labels = paste0(seq(0, 30, 5), "%")
  ) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  )+
  labs(x = "Local", y = "Frequência Relativa") + theme_bw()

#A5
D8A5<-Amostra50[Amostra50[,9]<75,]
c2A5 <- table(D8A5$REG, D8A5$LOCAL)%>%
  data.frame() %>%
  mutate(Pct = Freq / sum(Freq))


