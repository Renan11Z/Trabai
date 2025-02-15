source("rdocs/source/packages.R")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)

#carregar dados

dados<-read_excel("C:/Users/DESKTOP/Downloads/Amostra_g09_FelipeBretas_Renan_Tales_VictorSouza.xlsx")
dados50 <- dados[c(198, 196, 190, 189, 186, 173, 171, 169, 167,
                   154,147, 145, 144, 143, 141, 139, 132, 127,
                   117, 116, 111 , 99, 98, 96, 95, 90, 88, 79,
                   78, 77, 68,65,63, 60, 55, 52, 51, 48, 37, 35,
                   33, 31, 21, 19, 17, 13, 12, 8, 7, 6),]

# 6. Comparação das notas de Matemática entre escolas urbanas e rurais

t.test(NOTA_MT ~ LOCAL, data = dados)
t.test(NOTA_MT~ LOCAL, data = dados50)
# 7. Diferença significativa entre notas de Língua Portuguesa e Matemática
t.test(dados$NOTA_LP, dados$NOTA_MT, paired = TRUE)
t.test(dados50$NOTA_LP, dados50$NOTA_MT, paired = TRUE)
# 10. Associação entre notas de Língua Portuguesa e Matemática
cor.test(dados$NOTA_LP, dados$NOTA_MT)
cor.test(dados50$NOTA_LP, dados50$NOTA_MT)
