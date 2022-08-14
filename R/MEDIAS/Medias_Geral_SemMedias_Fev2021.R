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
data = read.csv('indata/database_2_2021.csv', sep = ";", header = FALSE, encoding = 'UTF-8')

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

# Converter Valor para num√©rico
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

# Omitir valores nulos para n√£o dar ruim no c√°lculo das m√©dias
mydata = na.omit(mydata)

# Conforme informado pelo Chico, remover os ensaios R1, quando houver a duplicata, 
# portanto at√© o dia 11/11/2020 os ensaios a serem removidos s√£o: 27,36,28
# Isto de acordo com a planilha de Controle

# Para aplicar o %in% sempre precisamos ter o dplyr carregado?
library(dplyr)
# Retirar o Ensaio 06, pq era a referÍncia, mas agora passou a ser o ensaio 46
# Decis„o informada no dia 04/02/2021

# Outros ensaios que È n„o para considerar È o 49 e o 52. 
# EndereÁo do audio: C:\Users\vpb\IPT\Francisco Junior Batista Pedrosa (FIPT) - AglomeraÁ„o de grossos\!Ana\Pelotizacao\Boxplot\R\Dados_Originais\Resistencias\AUDIO
mydata = subset(mydata, !(Ensaio %in% c('E6','E27','E18','E13','E26','E36','E38','E28')))

mydata = subset(mydata, !(Ensaio %in% c('E46','E47','E48','E49','E50','E51','E52','E53','E54','E55')))

mydata$Ensaio = as.factor(droplevels(mydata$Ensaio))
levels(mydata$Ensaio)
str(mydata$Ensaio)
# Agora precisamos obter datasets com as medias de cada experimento
# -- Resistencia Verde, Seca e Queimada
# -- Porosidade Verde, Seca e Queimada
# -- Dispersao
# -- D10, D50, D90

# Verificar quais Experimentos est√£o dispon√?veis
levels(mydata$Experimento)

# Criar ID para cada amostra
mydata$ID = as.factor(paste(mydata$Minerio,
                  mydata$Dose1,mydata$Aditivo1,
                  mydata$Dose2,mydata$Aditivo2,
                  mydata$Dose3,mydata$Aditivo3, 
                  mydata$AB))
# Verificar o n√∫mero de n√?veis de IDS
str(mydata$ID)

# bypass em subset para visualizar o que acontece ao utilizar todo o dataset mydata
subdata = mydata


# Agora queremos plotar sem a mÈdia ---------------------------------------
library(dplyr)
# # Obter as medias respectivas de amostra por Experimento e ID
# medias = subdata %>%
#   group_by(Minerio, TS, 
#            Dose1, Aditivo1, Dose2, Aditivo2, Dose3, Aditivo3,
#            AB,
#            Experimento, SubExp, SubSubExp, ID) %>%
#   summarise_at(vars(Valor), list(name = mean, sd))
# nft = length(medias)
# names(medias)[nft-1] = 'media'
# names(medias)[nft] = 'desvpad'
# medias[nft] = round(medias[nft], 2)

# N√£o vamos trabalhar com Choque T?rmico, nem Cura UV


# BYPASS EM MEDIAS!!!!!!!!!!!!!!!!!!!!!!!!!!! -----------------------------

medias = subdata

levels(medias$SubExp)
medias = subset(medias, !(SubExp %in% c('Seca e Curada UV, Verde e Curada UV','Curada UV',
                                         'Choque TÈrmico')))
medias$SubSubExp = NULL

# N√£o vamos trabalhar com TS que n√£o seja PFM ou 1 mm
medias = subset(medias, (TS == '1 mm' & Minerio == 'PFNM') |
                  (Minerio == 'PFM' & Aditivo1 == 'Bentonita Nacional'))

# TESTE SPREAD ## FUN√á√ÉO LINDA MARAVILHOSA, RESOLVEU TODOS MEUS PROBLEMAS!!!
# Foi ent√£o utilizado o melt antes, depois o group_by com summarize para m√©dia e desvio padr√£o e aagora finalmente separando
# todas vari√°veis novmaente com a fun√ßao spread() do maravilhoso tidyr

# AGORA VAI SER GERAL, N√ÉO UTILIZAR DIVIS√ÉO POR GRANULOMETRIAS
# medias = subset(medias, AB == 'A')

# Criar a vari√°vel refer√™ncia de chave (os experimentos)
medias$Exp = paste(medias$Experimento,medias$SubExp)

# Ent√£o drop experimento
medias$Experimento = NULL
medias$SubExp = NULL
medias$desvpad = NULL

# Pacote que cont√©m o spread()
library(tidyr)
?spread()

# A chave utilizada, das vari√°veis, ser√° a vari√°vel criada Exp a partir de Experimento e SubExp
# O valor nesse caso s„o os valores dos experimentos
spreaddata = medias %>%
  spread(key = Exp, value = Valor)

medias = spreaddata

# Criar as fam√?lias -------------------------------------------------------
medias$Aditivos = as.factor(paste(medias$Aditivo1, medias$Aditivo2, medias$Aditivo3))
levels(medias$Aditivos)
medias$Familia = as.character(medias$Aditivos)

# Replicar para deixar com 6 registros as linhas do PFM e PFNM de apenas 0,5 % Bentonita Nacional
?rep
pfmlines = subset(medias, ID %in% c('PFM 0.50% Bentonita Nacional     B','PFM 0.50% Bentonita Nacional     A'))
pfnmlines = subset(medias, ID %in% c('PFNM 0.50% Bentonita Nacional     B','PFNM 0.50% Bentonita Nacional     A'))
# 
# pfmlines = pfmlines[rep(seq_len(nrow(pfmlines)),each = 2),]
# pfnmlines = pfnmlines[rep(seq_len(nrow(pfnmlines)),each = 2),]

pfmlines = do.call('rbind', replicate(2,pfmlines, simplify = FALSE))
pfnmlines = do.call('rbind', replicate(2,pfnmlines, simplify = FALSE))

# Juntar no dataset geral
medias = rbind(medias, pfmlines, pfnmlines)

# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
medias_new = 
  medias %>%
  mutate(
    Familia = case_when(
      str_detect(Aditivos, "Arkomon") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "PEO") ~ "Amido ou PEO",
      str_detect(Aditivos, "Amido") ~ "Amido ou PEO",
      str_detect(Aditivos, "Peridur") ~ "Peridur",
      str_detect(Aditivos, "Bentonita Nacional") ~ "ReferÍncias", # Esse AA est√° aqui para ser poss√?vel ordenar, no arrange do dplyr com eles em primeiro
      TRUE ~ Familia
    )
  )

medias_new$Familia = as.factor(medias_new$Familia)
levels(medias_new$Familia)

# Concertar o Peridur que tem Arkomon
medias_new$Familia = as.character(medias_new$Familia)
medias_new = 
  medias_new %>%
  mutate(
    Familia = case_when(
      str_detect(Aditivos, 'Peridur') ~ 'Peridur',
      TRUE ~ Familia
    )
  )

medias_new$Referencia = medias_new$Familia
medias_new = 
  medias_new %>%
  mutate(
    Referencia = case_when(
      str_detect(Familia, 'ReferÍncias') ~ 'Sim',
      str_detect(Familia, 'Arkomon ou TPP') ~ 'N„o',
      str_detect(Familia, 'Amido ou PEO')  ~ 'N„o',
      str_detect(Familia, 'Peridur') ~ 'N„o',
      TRUE ~ Referencia
    )
  )

# Reordenar de acordo com as fam√?lias para facilitar renomear l√° as refer√™ncias para ficarem em todas fam√?lias
medias = medias_new
medias = 
  medias %>%
  arrange(Familia, Minerio)

# # Criar a sequ√™ncia que ser√° atribu√?da √†s 12 primeiras linhas
# refs_family = rep(rep(c('Arkomon ou TPP','Amido ou PEO','Peridur'), each = 2),2)
# 
# #Atribuir estes nomes √†s 12 primeiras linhas
# medias[1:12,ncol(medias)-1] = refs_family
# 

# Reordenar as colunas
medias = medias[,c(1:17,33:35,18:32)]

# Para gravar em csv
df = medias

# Para realizar os plots
write.csv(df, file = 'outputdata/df_plot_AB_Referencias_Fev2021.csv', row.names = FALSE, na = '')

df_write = df

# 
# df_write[c(1:9,11:13)] = NULL
# write.csv(df_write, file = 'outputdata/df_AB_fev2021.csv', row.names = FALSE, na = '')

