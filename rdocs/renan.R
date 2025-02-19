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

#####################analise 3#########
#alpha = 0,95
#Estimador de LP
miLP <- round((mean(Dados$NOTA_LP) - (1.96 * sd(Dados$NOTA_LP)/sqrt(200))), 2)
msLP <- round((mean(Dados$NOTA_LP) + (1.96 * sd(Dados$NOTA_LP)/sqrt(200))), 2)

#Estimador de MT
miMT <- round((mean(Dados$NOTA_MT) - (1.96 * sd(Dados$NOTA_MT)/sqrt(200))), 2)
msMT <- round((mean(Dados$NOTA_MT) + (1.96 * sd(Dados$NOTA_MT)/sqrt(200))), 2)
#########################Analise 4#########
((sd(Dados$NOTA_LP)/sqrt(200))*1.645)+(184.3)
mean(Dados$NOTA_LP)
((sd(Dados$NOTA_MT)/sqrt(200))*1.645)+(204.3)
mean(Dados$NOTA_MT)
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

 


