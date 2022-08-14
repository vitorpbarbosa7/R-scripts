rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
# setwd('C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos/!Ana/Pelotizacao/Boxplot/R')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('indata/database_UV.csv', sep = ",", header = FALSE, encoding = 'UTF-8')

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

# Converter todas colunas para fatores
mydata[names(mydata[c(1:18)])] = lapply(mydata[names(mydata[c(1:18)])], factor)

# Converter Valor para numérico
mydata$Valor = as.numeric(as.character(mydata$Valor))
summary(mydata)
str(mydata)

# Conforme informado pelo Chico, remover os ensaios R1, quando houver a duplicata, 
# portanto até o dia 11/11/2020 os ensaios a serem removidos são: 27,36,28
# Isto de acordo com a planilha de Controle

# Para aplicar o %in% sempre precisamos ter o dplyr carregado?
library(dplyr)
# Não é nenhum de UV, então OK
mydata = subset(mydata, !(Ensaio %in% c('E27','E18','E13','E26','E36','E38')))
str(mydata$Ensaio)

# Criar ID para cada amostra
mydata$ID = as.factor(paste(mydata$Minerio,
                            mydata$Dose1,mydata$Aditivo1,
                            mydata$Dose2,mydata$Aditivo2,
                            mydata$Dose3,mydata$Aditivo3,
                            mydata$AB))
# Verificar o número de níveis de IDS
str(mydata$ID)

mydata$Amostra = as.factor(paste(mydata$Minerio,mydata$TS,
                                 mydata$Dose1,mydata$Aditivo1,'\n',
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3))
levels(mydata$Amostra)

mydata$Aditivo = as.factor(paste(mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3))

mydata$Aditivos = as.factor(paste(mydata$Aditivo1,"e",mydata$Aditivo2))

#Inverter a ordem dos n?veis deste fator para de acordo com o desejado no plot
mydata$Aditivo = fct_rev(mydata$Aditivo) 
mydata$Amostra = fct_rev(mydata$Amostra)
levels(mydata$Amostra)
mydata$Aditivos = fct_rev(mydata$Aditivos)
mydata$AB = fct_rev(mydata$AB)

#Converter para fator e n?merico as devidas colunas
# mydata[,c(-17)] = lapply(mydata[-c(17)], factor)
mydata$Valor = as.character(mydata$Valor)
mydata$Valor = as.numeric(mydata$Valor)

mydata$Experimento = as.factor(mydata$Experimento)

# Verificar quais Experimentos estão disponíveis
levels(mydata$Experimento)

# Queimada não interessa
levels(subdata$SubExp)
subdata = subset(mydata, !(SubExp %in% c('Queimada')))

subdata$Amostra = as.character(subdata$Amostra)
subdata$Amostra = as.factor(subdata$Amostra)
levels(subdata$Amostra)

subdata$SubExp = as.factor(as.character(subdata$SubExp))

subdata$SubExp = relevel(subdata$SubExp, ref = 'Verde')

levels(subdata$SubExp)

#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)

#Converter para fator e n?merico as devidas colunas
# subdata[,c(-14)] = lapply(subdata[-c(14)], factor)
subdata$Valor = as.character(subdata$Valor)
subdata$Valor = as.numeric(subdata$Valor)

subdata$Amostra = as.factor(subdata$Amostra)

# Subdata UV Ensaios para resistências------------------------------------------------------
# Primeiro os ensaios em disco, depois em outro arquivo o ensaio que não foi em disco, foi com as gotas
subdata = subset(subdata, Experimento == 'Resistencia' &
                 (Ensaio %in% c('E30','E31','E32')) & 
                   AB == 'A')

#Substituir os codigos A e B pelos tamanhos das pelotas reais
subdata$AB = sub("A","-16+12,5 mm",subdata$AB)
subdata$AB = sub("B","-12,5+9,5 mm",subdata$AB)

library(dplyr)
subdata = subdata[!is.na(subdata$Valor),]
#Para poder agrupar por SubSubExp que cont?m a especifica??o de temperaturas do choque t?rmico
medias = subdata %>%
  group_by(Ensaio, SubExp, SubSubExp, AB, Amostra) %>%
  summarise_at(vars(Valor), list(name = mean))
nft = length(medias)
names(medias)[nft] = 'media'
medias[nft] = round(medias[nft], 2)

medias$SubExp = as.factor(as.character(medias$SubExp))
medias$SubExp = relevel(medias$SubExp, ref = 'Verde')

A = "-16+12,5 mm"
B = "-12,5+9,5 mm"

subdata$TS = fct_rev(subdata$TS)

theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do  t?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # axis.text.x=element_blank(),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 10), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

names(subdata)[6] = 'Condição'
# Desenhar o plot ---------------------------------------------------------
g = ggplot(subdata, aes(x = Amostra, y = Valor)) + 
  geom_boxplot(aes(color = Condição, fill = Condição), alpha = 0.3, outlier.size = 0.8) + 
  # facet_grid(.~AB, scales = "free") + 
  geom_text(data = medias, aes(group = SubExp, label = media, y = media + 0.01),size =4,
            position = position_dodge(0.8)) +
  xlab("") + 
  ylab("Resistência à compressão (kgf/pelota)") + 
  # ggtitle(paste("Resistência das pelotas  de granulometria -16 mm + 12,5 mm
                # confeccionadas com resina")) +
  # ggtitle(paste("Resistência das pelotas  de granulometria -12,5 mm + 9,5 mm
                # confeccionadas com resina")) + 
  #scale_color_manual(values =  cores) + 
  #scale_fill_manual(values =  cores) + 
  theme  
dev.off()
g

tiff('outputdata/plots/CuraUV/Disco_A.tiff', units = 'in', width = 12, height = 8, res = 300)
g
dev.off()


