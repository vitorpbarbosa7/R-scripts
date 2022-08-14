rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
# setwd('C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos/!Ana/Pelotizacao/Boxplot/R')

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
mydata$Aditivos = fct_rev(mydata$Aditivos)
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

# Obter os datasets das medias de cada Experimento --------------------------------------------------------------------
df_resverde = (subset(mydata, 
                  (Experimento == 'N° de Quedas' & AB == "A" & TS == "1 mm") |
                    (Experimento == 'N° de Quedas' & AB == "A" & TS == "PFM" & Aditivo1 == "Bentonita Nacional")))

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

# Self Joins
del = c(10,11)
# Res_Verde_A
resverdea = subset(medias, Experimento == 'Resistência' & SubExp == 'Verde' & AB == 'A')
resverdea[del] = NULL
names(resverdea)[c(ncol(resverdea)-1,ncol(resverdea))] = c('Res_Verde_A_Media','Res_Verde_A_SD')

# Res_Seca_A
ressecaa = subset(medias, Experimento == 'Resistência' & SubExp == 'Seca' & AB == 'A')
ressecaa[del] = NULL
names(ressecaa)[c(ncol(ressecaa)-1,ncol(ressecaa))] = c('Res_Seca_A_Media','Res_Seca_A_SD')

# Res_Queimada_A
resqueia = subset(medias, Experimento == 'Resistência' & SubExp == 'Queimada' & AB == 'A')
resqueia[del] = NULL
names(resqueia)[c(ncol(resqueia)-1,ncol(resqueia))] = c('Res_Queimada_A_Media','Res_Queimada_A_SD')

# disp_A
dispa = subset(medias, Experimento == 'Dispersao' & AB == 'A') 
dispa[del] = NULL
names(dispa)[c(ncol(dispa)-1,ncol(dispa))] = c('Dispersao_Media_A','disp_sd_A')

# Res_Verde_B 
resverdeb = subset(medias, Experimento == 'Resistência' & SubExp == 'Verde' & AB == 'B')
resverdeb[del] = NULL
names(resverdeb)[c(ncol(resverdeb)-1,ncol(resverdeb))] = c('Res_Verde_B_Media','Res_Verde_B_SD')

# Res_Seca_B
ressecab = subset(medias, Experimento == 'Resistência' & SubExp == 'Seca' & AB == 'B')
ressecab[del] = NULL
names(ressecab)[c(ncol(ressecab)-1,ncol(ressecab))] = c('Res_Seca_B_Media','Res_Seca_B_SD')

# Res_Queimada_B
resqueib = subset(medias, Experimento == 'Resistência' & SubExp == 'Queimada' & AB == 'B')
resqueib[del] = NULL
names(resqueib)[c(ncol(resqueib)-1,ncol(resqueib))] = c('Res_Queimada_B_Media','Res_Queimada_B_SD')

# disp_B
dispb = subset(medias, Experimento == 'Dispersao' & AB == 'B') 
dispb[del] = NULL
names(dispb)[c(ncol(dispb)-1,ncol(dispb))] = c('Dispersao_Media_B','disp_sd_B')

# Join dos A
rm(df_A)
df_A = Reduce(function(x,y) merge(x,y, all.x = TRUE, all.y = FALSE, all = FALSE), 
            list(resverdea, 
                 ressecaa, dispa,
                 resqueia))

ggplot(data = df_A, mapping = aes(x = Dispersao_Media_A, y = Res_Verde_A_Media)) + 
  geom_point(mapping = aes(fill = ID,  colour = ID, 
                           size = Res_Seca_A_Media*10, alpha = Res_Queimada_A_Media))

ggplot(data = df_A, mapping = aes(x = Res_Verde_A_Media, y = Res_Queimada_A_Media)) + 
  geom_point(mapping = aes(fill = ID, colour= ID))

#
df_merge = merge(df_merge, resverdeb, all.x = TRUE, all.y = FALSE, all = FALSE)


df_merge = merge(df_merge, ressecaa, all.x = TRUE, all.y = FALSE, all = FALSE)
df_merge = merge(df_merge, ressecab, all.x = TRUE, all.y = FALSE, all = FALSE)
df_merge = merge(df_merge, resqueia, all.x = TRUE, all.y = FALSE, all = FALSE)
df_merge = merge(df_merge, resqueib, all.x = TRUE, all.y = FALSE, all = FALSE)









# Para plot
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'none',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do t?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # axis.text.x=element_text(colour="black", angle = 75, hjust = 1, size = 8),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face


#Substituir os c?digos A e B pelos tamanhos das pelotas reais
subdata$AB = sub("A","-16+12,5 mm",subdata$AB)
subdata$AB = sub("B","-12,5+9,5 mm",subdata$AB)
#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)
A = "-16+12,5 mm"
B = "-12,5+9,5 mm"

ggplot(subdata, aes(x = ))


g = ggplot(subdata, aes(x = Amostra, y = Valor)) + 
  geom_boxplot(aes(color = Amostra, fill = Amostra), alpha = 0.3, outlier.size = 0.8) + 
  coord_flip() + 
  #facet_grid(.~TS, scales = "free") + 
  geom_text(data = medias, aes(label = media, y = media + 0.01),size =4) +
  geom_hline(yintercept = vline, color = 'red', linetype = 'dashed', size = 1) + 
  xlab("") + 
  ylab("N° de quedas (-)") + 
  ggtitle(paste("N° de quedas das pelotas de granulometria",A)) + 
  #scale_color_manual(values =  cores) + 
  #scale_fill_manual(values =  cores) + 
  theme  
g

dim = read.csv('dimensions.csv')

tiff('imgs/facet/Workshop/Quedas_A.tiff', width = dim$w, height = dim$h, res = dim$res,
     units = 'in')
g
dev.off()

# 
# library(lattice)
# 
# bwplot(Amostra ~ Valor | Aditivo1, data = subdata,
#        scales = list(x = 'free'))
#        