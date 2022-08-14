rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')

originaldata = read.csv('outputdata/df_long.csv', sep = ',', stringsAsFactors = TRUE)

subdata_a = subset(originaldata, AB == 'A')
# Criar as tais famílias
subdata_a$Aditivos = as.factor(paste(subdata_a$Aditivo1, subdata_a$Aditivo2, subdata_a$Aditivo3))
levels(subdata_a$Aditivos)
subdata_a$Familia1 = as.character(subdata_a$Aditivos)

# Familia Bentonita e Dispersantes dispersantes

library(stringr)

str_detect(subdata_a$Aditivos, 'Arkomon')

# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
subdata_a_new = 
  subdata_a %>%
  mutate(
    Familia1 = case_when(
      str_detect(Aditivos, "Arkomon") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "PEO") ~ "Amido ou PEO",
      str_detect(Aditivos, "Amido") ~ "Amido ou PEO",
      str_detect(Aditivos, "Peridur") ~ "Peridur",
      TRUE ~ Familia1
    )
  )

subdata_a_new$Familia1 = as.factor(subdata_a_new$Familia1)
levels(subdata_a_new$Familia1)

# Familia 2 
# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
subdata_a_new$Familia2 = as.character(subdata_a$Aditivos)

levels(subdata_a_new$Familia2)

subdata_a_new = 
  subdata_a_new %>%
  mutate(
    Familia2 = case_when(
      str_detect(Aditivos, "Bentonita Nacional") ~ "Bentonita Nacional",
      str_detect(Aditivos, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "PEO") ~ "Amido ou PEO",
      str_detect(Aditivos, "Amido") ~ "Amido ou PEO",
      str_detect(Aditivo1, "Peridur") ~ "Peridur",
      TRUE ~ Familia2
    )
  )
subdata_a_new$Familia2 = as.factor(subdata_a_new$Familia2)
levels(subdata_a_new$Familia2)

# Reordenando 
subdata_a = subdata_a_new
subdata_a = subdata_a[, c(1:17,19:23,18)]

# TESTE SPREAD ## FUNÇÃO LINDA MARAVILHOSA, RESOLVEU TODOS MEUS PROBLEMAS!!!
# Foi então utilizado o melt antes, depois o group_by com summarize para média e desvio padrão e aagora finalmente separando
# todas variáveis novmaente com a funçao spread() do maravilhoso tidyr

# Criar a variável referência de chave (os experimentos)
subdata_a$Exp = paste(subdata_a$Experimento,subdata_a$SubExp)

# Remover colunas para a key ficar correta
subdata_a$Replica = NULL
subdata_a$Ensaio = NULL
# subdata_a$Replicata = NULL
subdata_a$Unidade = NULL

# Então drop experimento
subdata_a$Experimento = NULL
subdata_a$SubExp = NULL
subdata_a$desvpad = NULL

# Pacote que contém o spread()
library(tidyr)
?spread()

# A chave utilizada, das variáveis, será a variável criada Exp a partir de Experimento e SubExp
# O valor são as médias, que foram calculadas com o dplyr através de group_by e summarize
spreaddata = subdata_a %>%
  spread(key = Exp, value = Valor)
