rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/Analises/Pelotizacao/Boxplot/R')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('Planilhas//database//GERAL_Workshop_v6_VITOR.csv', sep = ",", header = FALSE)

#Transpor
data2 = transpose(data)
data2 = data2[-c(1),]
names(data2) = data[,1]
data2[23] = NULL

#Transforma??o de colunas em linhas dos valores de interesse (R1 at? R2)
library(reshape2) ##Biblioteca que cont?m o reshape para transformar de linhas para colunas
mydata = melt(data = data2,
              id.vars = names(data2[c(1:12)]),
              measure.vars = names(data2[-c(1:12)])
              )
names(mydata)[c(13:14)] = c("Replicata","Valor")
names(mydata)[6] = "AB"

# Substituirta
# Criar nova coluna porque esqueci no Excel: (fun??o paste permite isso )
mydata$Amostra = as.factor(paste(mydata$Minerio,mydata$TS,
                       mydata$Dose1,mydata$Aditivo1,
                       mydata$Dose2,mydata$Aditivo2))

mydata$Aditivo = as.factor(paste(mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2))

#Inverter a ordem dos n?veis deste fator para de acordo com o desejado no plot
mydata$Aditivo = fct_rev(mydata$Aditivo) 
mydata$Amostra = fct_rev(mydata$Amostra)
mydata$AB = fct_rev(mydata$AB)

# #Ordem dos fatores de A e B
# neworder = c("A","B")
# library(plyr)  ## or dplyr (transform -> mutate)
# mydata <- arrange(transform(mydata,
#                             AB=factor(AB,levels=neworder)),AB)




#Converter para fator e n?merico as devidas colunas
mydata[,c(-14)] = lapply(mydata[-c(14)], factor)
mydata$Valor = as.numeric(mydata$Valor)

# Plot --------------------------------------------------------------------
subdata = (subset(mydata, 
                  (Experimento == 'Resistência' & SubExp == 'Verde' & AB == "A" & TS == "1 mm") |
                    (Experimento == 'Resistência' &
                       SubExp == 'Verde' & AB == "A" & TS == "PFM" & Aditivo1 == "Bentonita")))

#Substituir os c?digos A e B pelos tamanhos das pelotas reais
subdata$AB = sub("A","-16+12,5 mm",subdata$AB)
subdata$AB = sub("B","-12,5+9,5 mm",subdata$AB)
#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)


#Converter para fator e n?merico as devidas colunas
subdata[,c(-14)] = lapply(subdata[-c(14)], factor)
subdata$Valor = as.numeric(subdata$Valor)

#subdata$TS = relevel(subdata$TS, "PFM")


library(dplyr)
#Para poder agrupar por SubSubExp que cont?m a especifica??o de temperaturas do choque t?rmico
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
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust =0.5), #Posi??o do t?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

A = "-16+12,5 mm"
B = "-12,5+9,5 mm"

library(RColorBrewer)

#New levels for colors

addlevels = c('0.50% Bentonita  ','0.50% Bentonita 0.04% TPP','0.50% Bentonita 0.04% Arkomon',
              '0.04% Peridur 0.02% NaOH','0.12% Peridur 0.02% NaOH','0.04% TPP  ','0.15% TPP  ',
              '0.04% Arkomon  ','0.15% Arkomon  ','0.04% PEO  ','0.12% PEO  ','0.04% Amido  ')

samplelevels = paste("PFNM 1 mm ",addlevels, sep = '')
bentonitalevel = c("PFM PFM 0.50% Bentonita  ")

samplelevels = c(bentonitalevel, samplelevels)

subdata$Aditivo = factor(subdata$Aditivo, levels = addlevels)
subdata$Amostra = factor(subdata$Amostra, levels = samplelevels)

cores = c('#0289ba',  '#00B0F0',  '#33E0FF',  '#C733FF',  '#841dab',  '#25FF2F',  '#14c91c',  '#FF0000',  '#b50202',  '#f257e9',  '#FF00F0',  '#FFC204'
)

subdata$TS = fct_rev(subdata$TS)

g = ggplot(subdata, aes(x = Amostra, y = Valor)) + 
  geom_boxplot(aes(color = Aditivo, fill = Aditivo), alpha = 0.3, outlier.size = 2) + 
  facet_grid(.~TS, scales = "free") + 
  geom_text(data = medias, aes(label = media, y = media + 0.01),size =3) +
  xlab("") + 
  ylab("Resistência (kgf/pelota)") + 
  ggtitle(paste("Resistência das pelotas","verdes","de granulometria",A)) + 
  scale_color_manual(values =  cores) + 
  scale_fill_manual(values =  cores) + 
  theme  
g

dim = read.csv('dimensions.csv')

tiff('imgs/facet/Workshop/Resistencia_Verde_A.tiff', width = dim$w, height = dim$h, res = dim$res)
g
dev.off()



