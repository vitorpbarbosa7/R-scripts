rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
# setwd('C:/Users/fjrpe/OneDrive - IPT/Aglomera??o de grossos/AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
# setwd('C:/Users/fredalmeida/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
# setwd('C:/Users/fjrpe/OneDrive - IPT/Aglomera??o de grossos/AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('indata/database.csv', sep = ",", header = FALSE, encoding = 'UTF-8')

#Transpor
data2 = transpose(data)
data2 = data2[-c(1),]
names(data2) = data[,1]
data2[c(28,29)] = NULL

# Transforma??o de colunas em linhas dos valores de interesse (R1 at? R2)
library(reshape2) ##Biblioteca que cont?m o reshape para transformar de linhas para colunas
mydata = melt(data = data2,
              id.vars = names(data2[c(1:17)]),
              measure.vars = names(data2[-c(1:17)])
)
names(mydata)[c(18:19)] = c("Replicata","Valor")
names(mydata)[8] = "AB"
names(mydata)[1] = 'Experimento'

# Converter todas colunas para fatores
mydata[names(mydata[c(1:18)])] = lapply(mydata[names(mydata[c(1:18)])], factor)

# Converter Valor para numérico
mydata$Valor = as.numeric(as.character(mydata$Valor))
summary(mydata)
str(mydata)
# Substituirta
# Criar nova coluna porque esqueci no Excel: (fun??o paste permite isso )
mydata$Amostra = as.factor(paste(mydata$Minerio,mydata$TS,
                                 mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3,
                                 mydata$Ensaio,mydata$Replica))

mydata$Aditivo = as.factor(paste(mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3))

# Inverter a ordem dos n?veis deste fator para de acordo com o desejado no plot
mydata$Aditivo = fct_rev(mydata$Aditivo) 
mydata$Amostra = fct_rev(mydata$Amostra)
mydata$AB = fct_rev(mydata$AB)

# Converter para fator e n?merico as devidas colunas
mydata$Valor = as.character(mydata$Valor)
mydata$Valor = as.numeric(mydata$Valor)

# Converter para fator o Experimento
mydata$Experimento = as.factor(mydata$Experimento)

# Omitir valores nulos para não dar ruim no cálculo das médias
mydata = na.omit(mydata)

# Conforme informado pelo Chico, remover os ensaios R1, quando houver a duplicata, 
# portanto até o dia 11/11/2020 os ensaios a serem removidos são: 27,36,28
# Isto de acordo com a planilha de Controle

# Para aplicar o %in% sempre precisamos ter o dplyr carregado?
library(dplyr)
mydata = subset(mydata, !(Ensaio %in% c('E27','E18','E13','E26','E36','E38','E28')))
str(mydata$Replica)

# Agora precisamos obter datasets com as medias de cada experimento
# -- Resistencia Verde, Seca e Queimada
# -- Porosidade Verde, Seca e Queimada
# -- Dispersao
# -- D10, D50, D90

# Verificar quais Experimentos estão disponíveis
levels(mydata$Experimento)

# Criar ID para cada amostra
mydata$ID = as.factor(paste(mydata$Minerio,
                  mydata$Dose1,mydata$Aditivo1,
                  mydata$Dose2,mydata$Aditivo2,
                  mydata$Dose3,mydata$Aditivo3,
                  mydata$AB))
# Verificar o número de níveis de IDS
str(mydata$ID)

# bypass em subset para visualizar o que acontece ao utilizar todo o dataset mydata
subdata = mydata

library(dplyr)
# Obter as medias respectivas de amostra por Experimento e ID
medias = subdata %>%
  group_by(Minerio, TS, 
           Dose1, Aditivo1, Dose2, Aditivo2, Dose3, Aditivo3,
           Experimento, SubExp, SubSubExp, ID) %>%
  summarise_at(vars(Valor), list(name = mean, sd))
nft = length(medias)
names(medias)[nft-1] = 'media'
names(medias)[nft] = 'desvpad'
medias[nft] = round(medias[nft], 2)

# Não vamos trabalhar com Choque T?rmico, nem Cura UV
levels(medias$SubExp)
medias = subset(medias, !(SubExp %in% c('Seca e Curada UV, Verde e Curada UV','Curada UV',
                                         'Choque Térmico')))
medias$SubSubExp = NULL

# Não vamos trabalhar com TS que não seja PFM ou 1 mm
medias = subset(medias, (TS %in% c('1 mm', 'PFM')))

# Por enquanto excluir PFM
medias = subset(medias, (TS %in% c('1 mm')))

# Criar as tais famílias
medias$Aditivos = as.factor(paste(medias$Aditivo1, medias$Aditivo2, medias$Aditivo3))
levels(medias$Aditivos)
medias$Familia1 = as.character(medias$Aditivos)

# Familia Bentonita e Dispersantes dispersantes

library(stringr)

str_detect(medias$Aditivos, 'Arkomon')

# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
medias_new = 
  medias %>%
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

medias_new$Familia1 = as.factor(medias_new$Familia1)
levels(medias_new$Familia1)

# Familia 2 
# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
medias_new$Familia2 = as.character(medias$Aditivos)

levels(medias_new$Familia2)

medias_new = 
  medias_new %>%
  mutate(
    Familia2 = case_when(
      str_detect(Aditivos, "Bentonita Nacional") ~ "Bentonita Nacional",
      str_detect(Aditivos, 'Arkomon') ~ 'Arkomon ou TPP',
      str_detect(Aditivos, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "PEO") ~ "Amido ou PEO",
      str_detect(Aditivos, "Amido") ~ "Amido ou PEO",
      str_detect(Aditivo1, "Peridur") ~ "Peridur",
      TRUE ~ Familia2
    )
  )
medias_new$Familia2 = as.factor(medias_new$Familia2)
levels(medias_new$Familia2)

# Reordenando 
medias = medias_new
medias = medias[, c(1:12,15,16,17,13,14)]

# TESTE SPREAD ## FUNÇÃO LINDA MARAVILHOSA, RESOLVEU TODOS MEUS PROBLEMAS!!!
# Foi então utilizado o melt antes, depois o group_by com summarize para média e desvio padrão e aagora finalmente separando
# todas variáveis novmaente com a funçao spread() do maravilhoso tidyr

# AGORA VAI SER GERAL, NÃO UTILIZAR DIVISÃO POR GRANULOMETRIAS
# medias = subset(medias, AB == 'A')

# Criar a variável referência de chave (os experimentos)
medias$Exp = paste(medias$Experimento,medias$SubExp)

# Então drop experimento
medias$Experimento = NULL
medias$SubExp = NULL
medias$desvpad = NULL

# Pacote que contém o spread()
library(tidyr)
?spread()

# A chave utilizada, das variáveis, será a variável criada Exp a partir de Experimento e SubExp
# O valor são as médias, que foram calculadas com o dplyr através de group_by e summarize
spreaddata = medias %>%
  spread(key = Exp, value = media)

# Para gravar em csv
df = spreaddata

# Para realizar os plots
write.csv(df, file = 'outputdata/df_plot_geral.csv', row.names = FALSE, na = '')

df_write = df

df_write[c(1:9,11:13)] = NULL
write.csv(df_write, file = 'outputdata/df_geral.csv', row.names = FALSE, na = '')