source("source/biblioteca/packages.R")

library(readxl)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
Dados<-read_excel("C:/Users/marqu/Documents/UNB/2 semestre/Trabalho/Trabai/Amostra_g09_FelipeBretas_Renan_Tales_VictorSouza.xlsx")

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

#####################analise 3#########
formula
