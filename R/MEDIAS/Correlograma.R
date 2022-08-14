rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
library(corrplot)

# SÓ É POSSÍVEL LER ESTE ARQUIVO APÓS TER EXECUTADO A ULTIMA VERSÃO DO MEDIAS_V? O QUAL GERA O ARQUIVO df_A.csv
data = read.csv('outputdata/df_A_2.csv')

#Remover as colunas que não são os dados médios
data[c(1,5:7,15)] = NULL

#Renomear colunas
names(data)
names(data) = c('D10','D50','D90','Grau de Dispersão','N° de Quedas',
                'Porosidade Queimada','Porosidade Seca','Porosidade Verde',
                'Resistência Queimada','Resistência Seca','Resistência Verde')

# Reorganizar a ordem
data = data[c(1:5,11,10,9,8,7,6)]

# Verificar se são dados numéricos
str(data)

# Computar as correlações
correlacao = cor(data, use = 'complete.obs')

# Plotar
tiff('outputdata/plots/Correlacao/corr_algumasvariaveis_A.tiff', width = 14, height = 12, units = 'in', res = 200)
corrplot(correlacao,
         method = 'number',
         tl.col = 'black',
         tl.cex = 1.5, 
         number.cex = 1.5)
dev.off()
