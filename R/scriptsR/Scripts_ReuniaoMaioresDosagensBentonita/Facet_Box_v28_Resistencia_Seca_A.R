rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R')
# setwd('C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos/!Ana/Pelotizacao/Boxplot/R')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('Planilhas//database//GERAL_Workshop_v6_VITOR.csv', sep = ",", header = FALSE, encoding = 'UTF-8')

#Transpor
data2 = transpose(data)
data2 = data2[-c(1),]
names(data2) = data[,1]
data2[c(28,29)] = NULL

#Transforma??o de colunas em linhas dos valores de interesse (R1 at? R2)
library(reshape2) ##Biblioteca que cont?m o reshape para transformar de linhas para colunas
mydata = melt(data = data2,
              id.vars = names(data2[c(1:17)]),
              measure.vars = names(data2[-c(1:17)])
)
names(mydata)[c(18:19)] = c("Replicata","Valor")
names(mydata)[8] = "AB"
names(mydata)[1] = 'Experimento'

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

mydata$Aditivos = as.factor(paste(mydata$Aditivo1,"e",mydata$Aditivo2))

#Inverter a ordem dos n?veis deste fator para de acordo com o desejado no plot
mydata$Aditivo = fct_rev(mydata$Aditivo) 
mydata$Amostra = fct_rev(mydata$Amostra)
mydata$Aditivos = fct_rev(mydata$Aditivos)
mydata$AB = fct_rev(mydata$AB)

# #Ordem dos fatores de A e B
# neworder = c("A","B")
# library(plyr)  ## or dplyr (transform -> mutate)
# mydata <- arrange(transform(mydata,
#                             AB=factor(AB,levels=neworder)),AB)




#Converter para fator e n?merico as devidas colunas
# mydata[,c(-17)] = lapply(mydata[-c(17)], factor)
mydata$Valor = as.character(mydata$Valor)
mydata$Valor = as.numeric(mydata$Valor)

# Plot --------------------------------------------------------------------
subdata = (subset(mydata, 
                  (Experimento == 'Resistência' & SubExp == 'Seca' & AB == "A" & TS == "1 mm") |
                    (Experimento == 'Resistência' &
                       SubExp == 'Seca' & AB == "A" & TS == "PFM" & Aditivo1 == "Bentonita Nacional")))

#Substituir os c?digos A e B pelos tamanhos das pelotas reais
subdata$AB = sub("A","-16+12,5 mm",subdata$AB)
subdata$AB = sub("B","-12,5+9,5 mm",subdata$AB)
#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)


#Converter para fator e n?merico as devidas colunas
# subdata[,c(-14)] = lapply(subdata[-c(14)], factor)
subdata$Valor = as.character(subdata$Valor)
subdata$Valor = as.numeric(subdata$Valor)

library(dplyr)
#Para poder agrupar por SubSubExp que cont?m a especifica??o de temperaturas do choque t?rmico
medias = subdata %>%
  group_by(Ensaio, TS, Amostra) %>%
  summarise_at(vars(Valor), list(name = mean))
nft = length(medias)
names(medias)[nft] = 'media'
medias[nft] = round(medias[nft], 2)

#Reorder for facet
#neworder = c("Seca","Seca")
#library(plyr)  ## or dplyr (transform -> mutate)
#subdata <- arrange(transform(subdata,
#SubSubExp=factor(SubSubExp,levels=neworder)),SubSubExp)

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

A = "-16+12,5 mm"
B = "-12,5+9,5 mm"

library(RColorBrewer)
nomes = names(subdata[-c(18,19,20,21)])
nomes
str(subdata$Unidade)
subdata[nomes] = lapply(subdata[nomes], factor)

subdata$TS = fct_rev(subdata$TS)
subdata$Aditivo1 = as.factor(subdata$Aditivo1)

levels(subdata$Aditivo1)
levels(subdata$Dose1)
subdata$Amostra = fct_rev(subdata$Amostra)
vline = as.numeric(medias[medias$TS == 'PFM',][4])

g = ggplot(subdata, aes(x = Amostra, y = Valor)) + 
  geom_boxplot(aes(color = Amostra, fill = Amostra), alpha = 0.3, outlier.size = 0.8) + 
  coord_flip() + 
  #facet_grid(.~TS, scales = "free") + 
  geom_text(data = medias, aes(label = media, y = media + 0.01),size =4) +
  geom_hline(yintercept = vline, color = 'red', linetype = 'dashed', size = 1) + 
  xlab("") + 
  ylab("Resistência (kgf/pelota)") + 
  ggtitle(paste("Resistência das pelotas","Secas","de granulometria",A)) + 
  #scale_color_manual(values =  cores) + 
  #scale_fill_manual(values =  cores) + 
  theme  
g

dim = read.csv('dimensions.csv')

tiff('imgs/facet/Workshop/Resistencia_Seca_A.tiff', width = dim$w, height = dim$h, res = dim$res,
     units = 'in')
g
dev.off()

# 
# library(lattice)
# 
# bwplot(Amostra ~ Valor | Aditivo1, data = subdata,
#        scales = list(x = 'free'))
#        