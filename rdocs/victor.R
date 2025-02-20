source("rdocs/source/packages.R")
library(tidyverse)

dados<-read_excel(
  "Amostra_g09_FelipeBretas_Renan_Tales_VictorSouza.xlsx")
dados50 <- dados[c(198, 196, 190, 189, 186, 173, 171, 169, 167,
                   154,147, 145, 144, 143, 141, 139, 132, 127,
                   117, 116, 111 , 99, 98, 96, 95, 90, 88, 79,
                   78, 77, 68,65,63, 60, 55, 52, 51, 48, 37, 35,
                   33, 31, 21, 19, 17, 13, 12, 8, 7, 6),]

# 6. Comparação das notas de Matemática entre escolas urbanas e rurais

## n=200
t.test(NOTA_MT ~ LOCAL, data = dados)
dados$LOCAL <- recode(dados$LOCAL, "1" = "Urbana", "2" = "Rural")
ggplot(dados) +
  aes(
    x =dados$LOCAL,
    y = dados$NOTA_MT
  ) +
  geom_boxplot(fill = c("lightblue"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Local", y = "Nota em Matemática") +
  theme_bw()

## n=50
t.test(NOTA_MT~ LOCAL, data = dados50)
dados50$LOCAL <- recode(dados50$LOCAL, "1" = "Urbana", "2" = "Rural")
ggplot(dados50) +
  aes(
    x =dados50$LOCAL,
    y = dados50$NOTA_MT
  ) +
  geom_boxplot(fill = c("lightblue"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Local", y = "Nota em Matemática") +
  theme_bw()

# 7. Diferença significativa entre notas de Língua Portuguesa e Matemática

## n=200
t.test(dados$NOTA_LP, dados$NOTA_MT, paired = TRUE)
variancia_lp <- var(dados$NOTA_LP, na.rm = TRUE)
variancia_mt <- var(dados$NOTA_MT, na.rm = TRUE)

novo= pivot_longer(dados, cols = c("NOTA_LP", "NOTA_MT"),
                   names_to = "Disciplina", values_to = "Nota")
novo$Disciplina= recode(novo$Disciplina,"NOTA_LP"= "Português", "NOTA_MT"="Matemática")
ggplot(novo) +
  aes(
    x = novo$Disciplina,
    y = novo$Nota
  ) +
  geom_boxplot(fill = c("lightblue"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Matérias", y = "Notas") +
  theme_bw()

## n=50
t.test(dados50$NOTA_LP, dados50$NOTA_MT, paired = TRUE)
novo50= pivot_longer(dados50, cols = c("NOTA_LP", "NOTA_MT"),
                   names_to = "Disciplina", values_to = "Nota")
novo50$Disciplina= recode(novo50$Disciplina,"NOTA_LP"= "Língua Portuguesa", "NOTA_MT"="Matemática")
ggplot(novo50) +
  aes(
    x = novo50$Disciplina,
    y = novo50$Nota
  ) +
  geom_boxplot(fill = c("lightblue"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Matérias", y = "Notas") +
  theme_bw()


# 10. Associação entre notas de Língua Portuguesa e Matemática

## n=200
cor.test(dados$NOTA_LP, dados$NOTA_MT)
ggplot(dados, aes(x = dados$NOTA_LP, y = dados$NOTA_MT)) +
  geom_jitter(colour = "lightblue", size = 3) +
  labs(
    x = "Notas em Português",
    y = "Notas em Matemática"
  ) +
  theme_bw()

## n=50
cor.test(dados50$NOTA_LP, dados50$NOTA_MT)
ggplot(dados50, aes(x = dados50$NOTA_LP, y = dados50$NOTA_MT)) +
  geom_jitter(colour = "lightblue", size = 3) +
  labs(
    x = "Notas em Português",
    y = "Notas em Matemática"
  ) +
  theme_bw()


# fazer as tabelas da 6,7 e depois colocar as analises e as coclusoes do teste
