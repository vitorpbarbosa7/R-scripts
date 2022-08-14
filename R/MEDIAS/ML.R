setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
rm(list=ls())

data = read.csv('outputdata/df_plot.csv')

data[c(1,2,9,10,11,12,13,15,17,19,21,23,25,27,29)] = NULL
dataoriginal = data

# Preparação dos dados para aplicar o algoritmo de predição ---------------
str(data)

# Verificar quais colunas estão como character e passar para factor
charcols = sapply(data, is.character)
data[,charcols] = lapply(data[,charcols], factor)

str(data)


# Árvore de decisão -------------------------------------------------------
library(party)

tree = ctree(Res_Verde_A_Media~., data = data)
plot(tree, type = 'extended')

# Retirando a variável Resistência seca:
data$Res_Seca_A_Media = NULL 

tree = ctree(Res_Verde_A_Media~., data = data)
plot(tree, method = 'extended')

data$Dose2 = NULL 

tree = ctree(Res_Verde_A_Media~., data = data)
plot(tree, method = 'extended')


# RandomForest -----------------------------------------------------------------------
library(randomForest)

df = dataoriginal

# Deletar Resistência Queimada
df$Res_Queimada_A_Media = NULL

#Remover linhas que estão faltando dados ainda 
df = df[!is.na(df$Dispersao_Media_A),]

# Fill na
df[is.na(df)] = 0

str(df)

rfModel = randomForest(Res_Verde_A_Media~.,
                       data = df)

print(rfModel)
plot(rfModel)

varImpPlot(rfModel)

# Ao retirar as Doses
df$Dose2 = NULL
rfModel = randomForest(Res_Verde_A_Media~.,
                       data = df)

print(rfModel)
plot(rfModel)

varImpPlot(rfModel)

