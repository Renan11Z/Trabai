source("rdocs/source/packages.R")

#Banco de dados 
Amostra_g09_200 <- read_excel(
  "Amostra_g09_FelipeBretas_Renan_Tales_VictorSouza.xlsx")
Amostra_g09_50 <- Amostra_g09_200[c(198, 196, 190, 189, 186, 173, 171, 169, 167,
                                    154,147, 145, 144, 143, 141, 139, 132, 127,
                                    117, 116, 111 , 99, 98, 96, 95, 90, 88, 79,
                                    78, 77, 68,65,63, 60, 55, 52, 51, 48, 37, 35,
                                    33, 31, 21, 19, 17, 13, 12, 8, 7, 6),]

# Análise 2 - Estimar proporção média < 75% de participação

p <- Amostra_g09_200[Amostra_g09_200$PARTICIPACAO<=75,9]
p75 <- length(p)/length(Amostra_g09_200$PARTICIPACAO)

p0 <- round((p75 - ( 1.96 * sqrt((p75 * (1 - p75))/ length(Amostra_g09_200$PARTICIPACAO))
                     )), 2)
p1 <- round((p75 + ( 1.96 * sqrt((p75 * (1 - p75))/ length(Amostra_g09_200$PARTICIPACAO))
)), 2)
