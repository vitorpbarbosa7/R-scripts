rm(list=ls())
# setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/lattice')
# setwd('C:/Users/fjrpe/OneDrive - IPT/Aglomeração de grossos/AgloGrossos/!Ana/Pelotizacao/Boxplot/R/lattice')

# Para trabalhar com a base geral
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/Planilhas/database')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('GERAL_Workshop_v6_VITOR.csv', sep = ",", header = FALSE, encoding = 'UTF-8')

data2 = data
data2 = transpose(data2)
# Nomear header de acordo com primeira linha
names(data2) = data2[1,]
# Remover primeira linha para fazer com que a primeira seja o Header
data2 = data2[-c(1),] 

# Reshape
library(reshape2)

mydata = melt(data = data2,
              id.vars = names(data2[c(1:17)]),
              measure.vars = names(data2[c(18:27)]))
names(mydata)[length(mydata)-1] = 'Replicata'
names(mydata)[length(mydata)] = 'Valor'
names(mydata)[1] = 'Experimento'

#Valor como numeric:
mydata$Valor = as.numeric(mydata$Valor)

# Transformar todos em fatores
cols = names(mydata)
cols
class(cols)
remover = 'Valor'
cols2 = setdiff(cols,remover)
cols2
mydata[cols2] = lapply(X = mydata[cols2],FUN = factor)
summary(mydata)

levels(mydata$Experimento)

#Criar a variável ID
mydata$ID = paste(mydata$Replica,mydata$TS,
                  mydata$Dose1,mydata$Aditivo1,
                  mydata$Dose2,mydata$Aditivo2,
                  mydata$Dose3,mydata$Aditivo3,
                  mydata$Minerio,mydata$Replicata,
                  sep = '')

resverde = subset(mydata, Experimento == 'Resistencia' & SubExp == 'Verde' & TS == '1 mm')
names(resverde)[ncol(resverde)] = 'Res_Verde'

porverde = subset(mydata, Experimento == 'Porosidade' & SubExp == 'Verde' & TS == '1 mm')
names(porverde)[ncol(porverde)] = 'Por_Verde'

df_verde = merge(resverde, porverde, by = 'ID', all = TRUE)

resseca = subset(mydata, Experimento == 'Resistencia' & SubExp == 'Seca' & TS == '1 mm')
names(resseca)[ncol(resseca)] = 'Res_Seca'

porseca = subset(mydata, Experimento == 'Porosidade' & SubExp == 'Seca' & TS == '1 mm')
names(porseca)[ncol(porseca)] = 'Por_Seca'

df_seca = merge(resseca, porseca, by = 'ID', all = TRUE)

resquei = subset(mydata, Experimento == 'Resistencia' & SubExp == 'Queimada' & TS == '1 mm')
names(resquei)[ncol(resquei)] = 'Res_Queimada'

porquei = subset(mydata, Experimento == 'Porosidade' & SubExp == 'Queimada' & TS == '1 mm')
names(porquei)[ncol(porquei)] = 'Por_Queimada'

df_quei = merge(resquei, porquei, by = 'ID', all = TRUE)

# Geralzao no qual estão todas as Resistencia e porosidades na granulometria de 1 mm, sem distinção de 
# Verde, Seca ou Queimada
res = subset(mydata, Experimento == 'Resistencia' & TS == '1 mm')
names(res)[ncol(res)] = 'Resistencia'
por = subset(mydata, Experimento == 'Porosidade' & TS == '1 mm')
names(por)[ncol(por)] = 'Porosidade'

df_geral = merge(res, por, by = 'ID', all = TRUE)

# Inner join
?Reduce
df = Reduce(function(x, y) merge(x, y, all=TRUE, by = 'ID'), list(resverde, resseca, resquei,
                                                                     porverde, porseca, porquei))
names(df)

# verde_seca_quei = merge(resverde, resseca, by = 'ID')

library(lattice)

#Geral
xyplot(Resistencia ~ Porosidade | SubExp.x, data = df_geral, 
       groups = Aditivo1.x,
       auto.key = TRUE, 
       pch = 16,
       scales = list(y=list(relation='free')))


df_verde_seca = subset(df_geral, SubExp.x == 'Verde' | SubExp.x == 'Seca')

# Verde_seca
xyplot(Resistencia ~ Porosidade | SubExp.x, data = df_verde_seca,
       groups = Aditivo1.x,
       auto.key = TRUE,
       pch = 16,
       scales = list(y=list(relation = 'free')))

bwplot(Porosidade ~ ID | SubExp.x, data = df_verde_seca,
       scales = list(y=list(relation = 'free')),
       groups = Aditivo1.x,
       auto.key = TRUE, 
       pch = 16)

# Verde
summary(df_verde)

xyplot(Res_Verde ~ Por_Verde | TS.x + Minerio.x, data =df_verde,
       groups = Aditivo1.x,
       auto.key = TRUE,
       pch = 16)

xyplot(Res_Verde ~ Por_Verde | Aditivo1.x, data =df_verde)

xyplot(Res_Verde ~ Por_Verde | Aditivo1.x, data =df_verde,
       groups = TS.x,
       pch = 16,
       auto.key = TRUE)

# Seca
xyplot(Res_Seca ~ Por_Seca | TS.x + Minerio.x, data =df_seca, 
       groups = Aditivo1.x,
       auto.key = TRUE, 
       pch = 16)


# Queimada
xyplot(Res_Quei ~ Por_Quei, data = df_quei,
       groups = Aditivo1.x | TS.x + Minerio.x,
       auto,key = TRUE)