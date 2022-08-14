rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
# setwd('C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos/!Ana/Pelotizacao/Boxplot/R')
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
mydata = subset(mydata, !(Ensaio %in% c('E27','E36','E38')))
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
           AB,
           Experimento, SubExp, SubSubExp, ID) %>%
  summarise_at(vars(Valor), list(name = mean, sd))
nft = length(medias)
names(medias)[nft-1] = 'media'
names(medias)[nft] = 'desvpad'
medias[nft] = round(medias[nft], 2)

# Não vamos trabalhar com Choque Térmico, nem Cura UV
levels(medias$SubExp)
medias = subset(medias, !(SubExp %in% c('Seca e Curada UV, Verde e Curada UV','Curada UV',
                                         'Choque Térmico')))
medias$SubSubExp = NULL

# Não vamos trabalhar com TS que não seja PFM ou 1 mm
medias = subset(medias, (TS %in% c('1 mm', 'PFM')))

# Por enquanto excluir PFM
medias = subset(medias, (TS %in% c('1 mm')))

# Criar as tais famílias
medias$Familia = as.factor(paste(medias$Aditivo1, medias$Aditivo2))
levels(medias$Familia)

# Familia Bentonita e Dispersantes dispersantes
medias$Familia = sub('Bentonita Nacional TPP', 'Bentonita Nacional - Dispersante', medias$Familia)
medias$Familia = sub('Bentonita Nacional Arkomon', 'Bentonita Nacional - Dispersante', medias$Familia)
medias$Familia = as.factor(medias$Familia)
levels(medias$Familia)

# Familia Dispersantes
medias$Familia = sub('Arkomon ', 'Arkomon ou TPP', medias$Familia)
medias$Familia = sub('TPP ', 'Arkomon ou TPP', medias$Familia)
medias$Familia = as.factor(medias$Familia)
levels(medias$Familia)

# Reordenando 
medias = medias[, c(1:12,15,13,14)]

# Self Joins
del = c(10,11)
# Res_Verde_A
resverdea = subset(medias, Experimento == 'Resistencia' & SubExp == 'Verde' & AB == 'A')
resverdea[del] = NULL
names(resverdea)[c(ncol(resverdea)-1,ncol(resverdea))] = c('Res_Verde_A_Media','Res_Verde_A_SD')

# Res_Seca_A
ressecaa = subset(medias, Experimento == 'Resistencia' & SubExp == 'Seca' & AB == 'A')
ressecaa[del] = NULL
names(ressecaa)[c(ncol(ressecaa)-1,ncol(ressecaa))] = c('Res_Seca_A_Media','Res_Seca_A_SD')

# Res_Queimada_A
resqueia = subset(medias, Experimento == 'Resistencia' & SubExp == 'Queimada' & AB == 'A')
resqueia[del] = NULL
names(resqueia)[c(ncol(resqueia)-1,ncol(resqueia))] = c('Res_Queimada_A_Media','Res_Queimada_A_SD')

# D10_A
d10a = subset(medias, Experimento == 'D10' & AB == 'A')
d10a[del] = NULL
names(d10a)[c(ncol(d10a)-1,ncol(d10a))] = c('D10_A_Media','D10_A_SD')

# D50_A
d50a = subset(medias, Experimento == 'D50' & AB == 'A')
d50a[del] = NULL
names(d50a)[c(ncol(d50a)-1,ncol(d50a))] = c('D50_A_Media','D50_A_SD')

# D90_A
d90a = subset(medias, Experimento == 'D90' & AB == 'A')
d90a[del] = NULL
names(d90a)[c(ncol(d90a)-1,ncol(d90a))] = c('D90_A_Media','D90_A_SD')

# disp_A
dispa = subset(medias, Experimento == 'Dispersao' & AB == 'A') 
dispa[del] = NULL
names(dispa)[c(ncol(dispa)-1,ncol(dispa))] = c('Dispersao_Media_A','disp_sd_A')


# B

# Res_Verde_B 
resverdeb = subset(medias, Experimento == 'Resistencia' & SubExp == 'Verde' & AB == 'B')
resverdeb[del] = NULL
names(resverdeb)[c(ncol(resverdeb)-1,ncol(resverdeb))] = c('Res_Verde_B_Media','Res_Verde_B_SD')

# Res_Seca_B
ressecab = subset(medias, Experimento == 'Resistencia' & SubExp == 'Seca' & AB == 'B')
ressecab[del] = NULL
names(ressecab)[c(ncol(ressecab)-1,ncol(ressecab))] = c('Res_Seca_B_Media','Res_Seca_B_SD')

# Res_Queimada_B
resqueib = subset(medias, Experimento == 'Resistencia' & SubExp == 'Queimada' & AB == 'B')
resqueib[del] = NULL
names(resqueib)[c(ncol(resqueib)-1,ncol(resqueib))] = c('Res_Queimada_B_Media','Res_Queimada_B_SD')

# disp_B
dispb = subset(medias, Experimento == 'Dispersao' & AB == 'B') 
dispb[del] = NULL
names(dispb)[c(ncol(dispb)-1,ncol(dispb))] = c('Dispersao_Media_B','disp_sd_B')

# D10_B
d10b = subset(medias, Experimento == 'D10' & AB == 'B')
d10b[del] = NULL
names(d10b)[c(ncol(d10b)-1,ncol(d10b))] = c('D10_B_Media','D10_B_SD')

# D50_B
d50b = subset(medias, Experimento == 'D50' & AB == 'B')
d50b[del] = NULL
names(d50b)[c(ncol(d50b)-1,ncol(d50b))] = c('D50_B_Media','D50_B_SD')

# D90_B
d90b = subset(medias, Experimento == 'D90' & AB == 'B')
d90b[del] = NULL
names(d90b)[c(ncol(d90b)-1,ncol(d90b))] = c('D90_B_Media','D90_B_SD')

# Join dos A
rm(df_A)
df_A = Reduce(function(x,y) merge(x,y, all.x = TRUE, all.y = FALSE, all = FALSE), 
            list(resverdea, 
                 ressecaa, dispa,d10a,d50a,d90a,
                 resqueia))

df_write = df_A
df_write[c(1:8,11,13,15,17,19,21,23)] = NULL
write.csv(df_write, file = 'outputdata/df_A.csv', row.names = FALSE, na = '')

# Para plot
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do t?tulo
        #panel.border = element_blank(), panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # axis.text.x=element_text(colour="black", angle = 75, hjust = 1, size = 8),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

# Plots de A
size = 5
# Verde x Dispersao
ggplot(data = df_A, mapping = aes(x = Dispersao_Media_A, y = Res_Verde_A_Media)) + 
  geom_point(mapping = aes(fill = ID,  colour = ID, shape = ID), size = size) + 
  scale_shape_manual(values=1:nlevels(df_A$ID)) + 
  geom_errorbar(aes(ymin = Res_Verde_A_Media - Res_Verde_A_SD,
                    ymax = Res_Verde_A_Media + Res_Verde_A_SD)) + 
  facet_wrap(.~Familia) + 
  theme

# Seca x Dispersao
ggplot(data = df_A, mapping = aes(x = Dispersao_Media_A, y = Res_Seca_A_Media)) + 
  geom_point(mapping = aes(fill = ID,  colour = ID, shape = ID), size = size) + 
  scale_shape_manual(values=1:nlevels(df_A$ID)) + 
  facet_wrap(.~Familia) + 
  theme

# Queimada x Dispersao
ggplot(data = df_A, mapping = aes(x = Dispersao_Media_A, y = Res_Queimada_A_Media)) + 
  geom_point(mapping = aes(fill = ID,  colour = ID, shape = ID), size = size) + 
  scale_shape_manual(values=1:nlevels(df_A$ID)) + 
  facet_wrap(.~Familia) + 
  theme

# D10_A x Dispersao
ggplot(data = df_A, mapping = aes(x = Dispersao_Media_A, y = D10_A_Media)) + 
  geom_point(mapping = aes(fill = ID,  colour = ID, shape = ID), size = size) + 
  scale_shape_manual(values=1:nlevels(df_A$ID)) + 
  facet_wrap(.~Familia) + 
  theme

# D90_A x Dispersao
ggplot(data = df_A, mapping = aes(x = Dispersao_Media_A, y = D90_A_Media)) + 
  geom_point(mapping = aes(fill = ID,  colour = ID, shape = ID), size = size) + 
  scale_shape_manual(values=1:nlevels(df_A$ID)) + 
  facet_wrap(.~Familia) + 
  theme

# Resis_Verde_A x D90_A
ggplot(data = df_A, mapping = aes(x = D90_A_Media, y = Res_Verde_A_Media)) + 
  geom_point(mapping = aes(fill = ID,  colour = ID, shape = ID), size = size) + 
  scale_shape_manual(values=1:nlevels(df_A$ID)) + 
  facet_wrap(.~Familia) + 
  geom_errorbar(aes(ymin = Res_Verde_A_Media - Res_Verde_A_SD,
                    ymax = Res_Verde_A_Media + Res_Verde_A_SD)) + 
  xlab('D90 (µm)') + 
  ylab('Resistencia (kgf/pelota)')

# Verde x Seca
ggplot(data = df_A, mapping = aes(x = Res_Verde_A_Media, y = Res_Seca_A_Media)) + 
  geom_point(mapping = aes(fill = ID,  colour = ID, shape = ID), size = size) + 
  scale_shape_manual(values=1:nlevels(df_A$ID)) + 
  facet_wrap(.~Familia) + 
  theme



#Substituir os c?digos A e B pelos tamanhos das pelotas reais
subdata$AB = sub("A","-16+12,5 mm",subdata$AB)
subdata$AB = sub("B","-12,5+9,5 mm",subdata$AB)
#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)
A = "-16+12,5 mm"
B = "-12,5+9,5 mm"