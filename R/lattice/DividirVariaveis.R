rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/lattice')
setwd('C:/Users/fjrpe/OneDrive - IPT/Aglomeração de grossos/AgloGrossos/!Ana/Pelotizacao/Boxplot/R/lattice')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('data2.csv', sep = ",", header = TRUE, encoding = 'UTF-8')
names(data)[1] = 'Experimento'

mydata = data

cols = names(mydata)
cols
class(cols)
remover = 'Valor'
cols2 = setdiff(cols,remover)
cols2
mydata[cols2] = lapply(X = mydata[cols2],FUN = factor)
summary(mydata)

levels(mydata$Experimento)

resverde = subset(mydata,
                            Experimento == 'Resistência' & SubExp == 'Verde')
names(resverde)[ncol(resverde)] = 'Res_Verde'

posverde = subset(mydata,
                  Experimento == 'Porosidade' & SubExp == 'Verde')
names(posverde)[ncol(posverde)] = 'Pos_Verde'

resseca = subset(mydata,
                           Experimento == 'Resistência' & SubExp == 'Seca')
names(resseca)[ncol(resseca)] = 'Res_Seca'

posseca = subset(mydata,
                 Experimento == 'Porosidade' & SubExp == 'Seca')
names(posseca)[ncol(posseca)] = 'Pos_Seca'

resquei = subset(mydata,
                 Experimento == 'Resistência' & SubExp == 'Queimada')
names(resquei)[ncol(resquei)] = 'Res_Queimada'

posquei = subset(mydata,
                 Experimento == 'Porosidade' & SubExp == 'Queimada')
names(posquei)[ncol(posquei)] = 'Pos_Queimada'

# Inner join
?Reduce
df = Reduce(function(x, y) merge(x, y, all=TRUE, by = 'ID'), list(resverde, resseca, resquei,
                                                                     posverde, posseca, posquei))
names(df)

# verde_seca_quei = merge(resverde, resseca, by = 'ID')

library(lattice)

# Verde

xyplot(Res_Verde ~ Pos_Verde, data =df)

xyplot(Res_Verde ~ Pos_Verde, data =df,
       groups = TS.x,
       auto.key = TRUE)

xyplot(Res_Verde ~ Pos_Verde | Aditivo1.x, data =df)

xyplot(Res_Verde ~ Pos_Verde | Aditivo1.x, data =df,
       groups = TS.x,
       pch = 16,
       auto.key = TRUE)

xyplot(Res_Verde ~ Pos_Verde | Aditivo1.x + TS.x, data =df)

# Seca
xyplot(Res_Seca ~ Pos_Seca, data =df, 
       groups = TS.x,
       auto.key = TRUE)

