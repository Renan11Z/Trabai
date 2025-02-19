source("rdocs/source/packages.R")

#Banco de dados 
Amostra_g09_200 <- read_excel(
  "Amostra_g09_FelipeBretas_Renan_Tales_VictorSouza.xlsx")
Amostra_g09_50 <- Amostra_g09_200[c(198, 196, 190, 189, 186, 173, 171, 169, 167,
                                    154,147, 145, 144, 143, 141, 139, 132, 127,
                                    117, 116, 111 , 99, 98, 96, 95, 90, 88, 79,
                                    78, 77, 68,65,63, 60, 55, 52, 51, 48, 37, 35,
                                    33, 31, 21, 19, 17, 13, 12, 8, 7, 6),]

# Análise 1 - Análise descritiva das escolas e do desempenhos dos alunos

# gráfico hist das notas

ggplot(Amostra_g09_200) +
  aes(x = NOTA_LP) +
  geom_histogram(colour = "white", fill = "lightblue", binwidth = 7) +
  labs(x = "Notas Língua Portuguesa", y = "Frequência Absoluta") + 
  theme_bw()

ggplot(Amostra_g09_200) +
  aes(x = NOTA_MT) +
  geom_histogram(colour = "white", fill = "lightblue", binwidth = 7) +
  labs(x = "Notas Matemática", y = "Frequência Absoluta") + 
  theme_bw()



# região e cat administrativa



# Análise 2 - Estimar proporção média < 75% de participação

p200 <- Amostra_g09_200[Amostra_g09_200$PARTICIPACAO<=75,9]
p75 <- length(p200)/length(Amostra_g09_200$PARTICIPACAO)

p0 <- round((p75 - ( 1.96 * sqrt((p75 * (1 - p75))/ length(Amostra_g09_200$PARTICIPACAO))
                     )), 2)
p1 <- round((p75 + ( 1.96 * sqrt((p75 * (1 - p75))/ length(Amostra_g09_200$PARTICIPACAO))
)), 2)

p50 <- Amostra_g09_50[Amostra_g09_50$PARTICIPACAO<=75,9]

p750 <- length(p50)/length(Amostra_g09_50$PARTICIPACAO)

p00 <- round((p750 - ( 1.96 * sqrt((p750 * (1 - p750))/ length(Amostra_g09_50$PARTICIPACAO))
)), 2)

p11 <- round((p750 + ( 1.96 * sqrt((p750 * (1 - p750))/ length(Amostra_g09_50$PARTICIPACAO))
)), 2)

# Análise 5 - LP e MAT são normalmente distribuídas?
#teste de aderência 

média_LP200 <- round(mean(Amostra_g09_200$NOTA_LP), 2)
variância_LP200 <- round(var(Amostra_g09_200$NOTA_LP), 2)

chisq.test(Amostra_g09_200$NOTA_LP, )


# Análise 9 - a) associação entre região e categoria administrativa 

analise_9a <- Amostra_g09_200[,c(3,6)]

analise_9a[analise_9a$ADM==1,2] <- "Federal"
analise_9a[analise_9a$ADM==2,2] <- "Estadual"
analise_9a[analise_9a$ADM==3,2] <- "Municipal"

#grafico 9 

a_9a <- analise_9a %>%
  mutate(ADM = case_when(
    ADM %>% str_detect("Estadual") ~ "Estadual",
    ADM %>% str_detect("Municipal") ~ "Municipal"
  )) %>%
  group_by(ADM, REG) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(a_9a$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(a_9a$freq, " (", porcentagens, ")"))

ggplot(a_9a) +
  aes(
    x = fct_reorder(ADM, freq, .desc = T),
    y = freq,
    fill = REG,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Categoria Administrativa", y = "Frequência") +
  scale_fill_brewer("Região", palette = "Blues") +
  theme_bw()

chisq.test(analise_9a$REG, analise_9a$ADM)

x2_9a <- 11.51

C_9a <- sqrt(x2_9 / (x2_9 + 200))

Cmax_9a <- sqrt(1/2)

Cmod_9a <- C_9/Cmax_9

#associação de fraca a moderada

#Analise 9 - b) associação tamanho da escola e tamanho do municipio

analise_9b <- Amostra_g09_200[,c(5,7)]

analise_9b$TAM_ESCOLA <- as.character(analise_9b$TAM_ESCOLA)
analise_9b$TAM_MUN <- as.character(analise_9b$TAM_MUN)

analise_9b[analise_9b$TAM_ESCOLA==1,2] <- "<25"
analise_9b[analise_9b$TAM_ESCOLA==2,2] <- "25-49"
analise_9b[analise_9b$TAM_ESCOLA==3,2] <- "50-99"
analise_9b[analise_9b$TAM_ESCOLA==4,2] <- "100+"

analise_9b[analise_9b$TAM_MUN==1,1] <- "<20000"
analise_9b[analise_9b$TAM_MUN==2,1] <- "20000-49999"
analise_9b[analise_9b$TAM_MUN==3,1] <- "50000-99999"
analise_9b[analise_9b$TAM_MUN==4,1] <- "100000-999999"
analise_9b[analise_9b$TAM_MUN==5,1] <- "1000000+"

a_9b <- analise_9b %>%
  mutate(TAM_MUN = case_when(
    TAM_MUN %>% str_detect("<20000") ~ "<20000",
    TAM_MUN %>% str_detect("20000-49999") ~ "20000-49999",
    TAM_MUN %>% str_detect("50000-99999") ~ "50000-99999",
    TAM_MUN %>% str_detect("100000-999999") ~ "100000-999999",
    TAM_MUN %>% str_detect("1000000+") ~ "1000000+"
  )) %>%
  group_by(TAM_MUN, TAM_ESCOLA) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

a_9b <- a_9b %>%
  mutate(TAM_ESCOLA = case_when(
    TAM_ESCOLA %in% c("<25", "25-49") ~ "<50",  
    TRUE ~ TAM_ESCOLA  
  ))


a_9b$TAM_ESCOLA <- factor(a_9b$TAM_ESCOLA, 
                          levels = c("<50", "50-99", "100+"),
                          labels = c("<50", "50-99", "100+"),
                          ordered = TRUE) 

a_9b$TAM_MUN <- factor(a_9b$TAM_MUN, 
                       levels = c("<20000", "20000-49999", "50000-99999", "100000-999999", "1000000+"),
                       labels = c("<20000", "20000-49999", "50000-99999", "100000-999999", "1000000+"),
                       ordered = TRUE)

a_9b <- a_9b %>%
  arrange(TAM_MUN, TAM_ESCOLA)

a_9b <- a_9b %>%
  drop_na()

porcentagens_b <- str_c(a_9b$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas_b <- str_squish(str_c(a_9b$freq, " (", porcentagens_b, ")"))

ggplot(a_9b) +
  aes(
    x = TAM_MUN,  
    y = freq,
    fill = TAM_ESCOLA,
    label = legendas_b
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 2.5
  ) +
  labs(x = "Tamanho dos Municípios", y = "Frequência") +
  scale_fill_brewer("Tamanho da Escola", palette = "Blues") +
  theme_bw()

