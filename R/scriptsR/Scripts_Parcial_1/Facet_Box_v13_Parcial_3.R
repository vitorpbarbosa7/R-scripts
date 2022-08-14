setwd('C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomeração de grossos/Analises/Pelotizacao/Boxplot/R')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que contém o poderoso ggplot e read_csv e não o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('Planilhas//database//GERAL_v2_Parcial_3.csv', sep = ",", header = FALSE)

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
                       mydata$Dose2,mydata$Aditivo2,
                       sep = "-"))

#Substituir os códigos A e B pelos tamanhos das pelotas reais
mydata$AB = sub("A","-16+12,5 mm",mydata$AB)
mydata$AB = sub("B","-12,5+9,5 mm",mydata$AB)
#Remove the NAs returned (valores nulos que não existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)

#Ordem dos fatores de A e B
neworder = c("-16+12,5 mm","-12,5+9,5 mm")
library(plyr)  ## or dplyr (transform -> mutate)
mydata <- arrange(transform(mydata,
                             AB=factor(AB,levels=neworder)),AB)

#Converter para fator e númerico as devidas colunas
mydata[,c(-14)] = lapply(mydata[-c(14)], factor)
mydata$Valor = as.numeric(mydata$Valor)

# Plot --------------------------------------------------------------------
subdata = (subset(mydata, Experimento == 'Porosidade' &
                   SubExp == 'Verde' & TS == "1 mm"))


#Converter para fator e númerico as devidas colunas
subdata[,c(-14)] = lapply(subdata[-c(14)], factor)
subdata$Valor = as.numeric(subdata$Valor)


# library(dplyr)
# #Para poder agrupar por SubSubExp que contém a especificação de temperaturas do choque térmico
# medias = subdata %>%
#   group_by(TS, AB, Amostra) %>%
#   summarise_at(vars(Valor), list(name = mean))
# names(medias)[3] = 'media'
# medias[3] = round(medias[3], 1)

#Reorder for facet
#neworder = c("Verde","Seca")
#library(plyr)  ## or dplyr (transform -> mutate)
#subdata <- arrange(transform(subdata,
                           #SubSubExp=factor(SubSubExp,levels=neworder)),SubSubExp)

theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posição da legenda
        plot.title = element_text(hjust =0.5), #Posição do título
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        #axis.text.x=element_text(colour="black"),
        axis.text.x = element_blank(),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos títulos de cada face

#cores = rep(c("#FF0000","#0000FF","#FFA500","#800080","#00FF00"),5)

g = ggplot(subdata, aes(x = Amostra, y = Valor)) + 
  geom_boxplot(aes(color = Amostra), outlier.size = 0.8) + 
  #facet_wrap( scales = "free") + 
  #geom_text(data = medias, aes(label = media, y = media + 0.01),size =5) +
  xlab("") + 
  ylab("Porosidade (%)") + 
  theme  
g


