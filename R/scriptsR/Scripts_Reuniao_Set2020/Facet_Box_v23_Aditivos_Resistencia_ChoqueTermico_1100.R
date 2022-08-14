rm(list=ls())
setwd('C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomeração de grossos/Analises/Pelotizacao/Boxplot/R')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que contém o poderoso ggplot e read_csv e não o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('Planilhas//database//GERAL_Parcial3_v5.csv', sep = ",", header = FALSE)

#Transpor
data2 = transpose(data)
data2 = data2[-c(1),]
names(data2) = data[,1]

#Transformação de colunas em linhas dos valores de interesse (R1 até R2)
library(reshape2) ##Biblioteca que contém o reshape para transformar de linhas para colunas
mydata = melt(data = data2,
              id.vars = names(data2[c(1:12)]),
              measure.vars = names(data2[-c(1:12)])
              )
names(mydata)[c(13:14)] = c("Replicata","Valor")
names(mydata)[6] = "AB"

# Substituirta
# Criar nova coluna porque esqueci no Excel: (função paste permite isso )
mydata$Amostra = as.factor(paste(mydata$Minerio,mydata$TS,
                       mydata$Dose1,mydata$Aditivo1,
                       mydata$Dose2,mydata$Aditivo2))

mydata$Aditivo = as.factor(paste(mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2))

#Inverter a ordem dos níveis deste fator para de acordo com o desejado no plot
mydata$Aditivo = fct_rev(mydata$Aditivo) 
mydata$Amostra = fct_rev(mydata$Amostra)
mydata$AB = relevel(mydata$AB, "A")

# #Ordem dos fatores de A e B
# neworder = c("A","B")
# library(plyr)  ## or dplyr (transform -> mutate)
# mydata <- arrange(transform(mydata,
#                             AB=factor(AB,levels=neworder)),AB)

#Converter para fator e númerico as devidas colunas
mydata[,c(-14)] = lapply(mydata[-c(14)], factor)
mydata$Valor = as.numeric(mydata$Valor)

#Nome sem PFM:
library(plyr)
mydata$Amostra = revalue(mydata$Amostra, c("PFM PFM 0.50% Bentonita  " = "PFM 0.50% Bentonita",
                                          "PFM PFM 0.15% TPP  " = "PFM 0.15% TPP",
                                          "PFM PFM 0.15% Arkomon  " = "PFM 0.15% Arkomon",
                                          "PFM PFM 0.04% Peridur 0.02% NaOH" = "PFM 0.04% Peridur 0.02% NaOH"))

# Plot --------------------------------------------------------------------
mydata = mydata[!is.na(mydata$Valor),]

subdata = (subset(mydata, Experimento == 'Resistência' &
                   SubExp == 'Choque Térmico' & AB == "B" & SubSubExp == "1100 °C" &
                   (TS == "4,74 mm" | TS == "1 mm" | TS == "PFM")
                   )
			)

#Substituir os códigos A e B pelos tamanhos das pelotas reais
subdata$AB = sub("A","-16+12,5 mm",subdata$AB)
subdata$AB = sub("B","-12,5+9,5 mm",subdata$AB)
#Remove the NAs returned (valores nulos que não existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)

#Converter para fator e númerico as devidas colunas
subdata[,c(-14)] = lapply(subdata[-c(14)], factor)
subdata$Valor = as.numeric(subdata$Valor)

#Reordenar níveis
subdata$TS = relevel(subdata$TS, "PFM")
subdata$SubSubExp = ordered(subdata$SubSubExp, levels = c("300 °C","500 °C","700 °C","900 °C","1100 °C"))

library(dplyr)
#Para poder agrupar por SubSubExp que contém a especificação de temperaturas do choque térmico
medias = subdata %>%
  group_by(TS, Amostra) %>%
  summarise_at(vars(Valor), list(name = mean))

nft = length(medias)
names(medias)[nft] = 'media'
medias[nft] = round(medias[nft], 2)

#Reorder for facet
#neworder = c("Verde","Seca")
#library(plyr)  ## or dplyr (transform -> mutate)
#subdata <- arrange(transform(subdata,
                           #SubSubExp=factor(SubSubExp,levels=neworder)),SubSubExp)

theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posição da legenda
        plot.title = element_text(hjust =0.5), #Posição do título
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos títulos de cada face

cores = rep(c("#00B0F0","#92D050","#FF0000","#7030A0"),4)
A = "-16+12,5 mm"
B = "-12,5+9,5 mm"

g = ggplot(subdata, aes(x = Amostra, y = Valor)) + 
  geom_boxplot(aes(color = Aditivo), outlier.size = 0.8) + 
  facet_wrap(.~TS, scales = "free") + 
  geom_text(data = medias, aes(label = media, y = media + 0.01),size =5) +
  xlab("") + 
  ylab(paste("Resistência","(kgf/pelota)")) + 
  ggtitle(paste("Granulometria: -12,5 mm +9,5 mm -",
                "Choque Térmico: 1100 °C")) + 
  scale_color_manual(values = cores, breaks = levels(factor(subdata$Aditivo))) + 
  theme  
g

dim = read.csv('dimensions.csv')

tiff('imgs/facet/Parcial_3/Resistencia_ChoqueTermico_1100.tiff', width = dim$w, height = dim$h, res = dim$res)
g
dev.off()




