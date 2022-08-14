setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
rm(list=ls())

# setwd('C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos/!Ana/Pelotizacao/Boxplot/R')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(data.table) #Para transpor os dados
library(lemon)

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('indata/pelletizacao.csv', sep = ",", header = FALSE, encoding = 'UTF-8')

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
names(mydata)[1] = 'Experimento'

# Converter todas colunas para fatores
mydata[names(mydata[c(1:18)])] = lapply(mydata[names(mydata[c(1:18)])], factor)

# Converter Valor para numérico
mydata$Valor = as.numeric(as.character(mydata$Valor))
summary(mydata)
str(mydata)

# Criar ID para cada amostra
mydata$ID = as.factor(paste(mydata$Minerio,
                            mydata$Dose1,mydata$Aditivo1,
                            mydata$Dose2,mydata$Aditivo2,
                            mydata$Dose3,mydata$Aditivo3,
                            mydata$AB))
# Verificar o número de níveis de IDS
str(mydata$ID)

mydata$Amostra = as.factor(paste(mydata$Minerio,mydata$TS,
                                 mydata$Dose1,mydata$Aditivo1,
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

subdata = mydata

table(subdata$SubExp)

# Verificar se selecionou todos os TS desejados
table(subdata$TS)
# Verificar se selecionou corretamente os Experimentos
table(subdata$Experimento)

subdata = subset(subdata, Experimento == 'Resistencia')

subdata$SubExp = as.factor(droplevels(subdata$SubExp))
levels(subdata$SubExp)


# Mudar ordem dos fatores -------------------------------------------------
#  Ordem das amostras para apresentação no boxplot ------------------------
# Ordem das amostras
levels(subdata$Amostra)
table(subdata$Amostra)

# Remover níveis com contagem zero 
?droplevels
subdata$Amostra = droplevels(subdata$Amostra)

levels(subdata$Amostra)

# Remover espaços no final
subdata$Amostra = as.factor(trimws(subdata$Amostra, which = c('right')))
levels(subdata$Amostra)

# Mudar ordem de Bentonita Nacional e Peridur na respectiva amostra
library(stringr)
subdata$Amostra = as.factor(str_replace(subdata$Amostra,
                                        "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur 0.02% NaOH",
                                        "PFNM 1 mm 0.06% Peridur 0.02% NaOH \n 0.50% Bentonita Nacional"))
levels(subdata$Amostra)

# Médias para o aparecer no boxplot

medias = subdata %>%
  group_by(Experimento, Minerio, SubExp, Amostra) %>%
  summarise_at(vars(Valor), list(name = mean))
nft = length(medias)
names(medias)[nft] = 'media'
medias[nft] = round(medias[nft], 2)

# Plot --------------------------------------------------------------------
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do t?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

subdata$TS = fct_rev(subdata$TS)
subdata$Aditivo1 = as.factor(subdata$Aditivo1)

levels(subdata$Aditivo1)
levels(subdata$Dose1)
subdata$Amostra = fct_rev(subdata$Amostra)

subdata$SubExp = fct_rev(subdata$SubExp)
levels(subdata$SubExp)


# Resistencias ------------------------------------------------------------

gres = ggplot(subdata, aes(x = Amostra, y = Valor)) + 
  geom_boxplot(aes(color = Amostra, fill = Amostra), alpha = 0.5, outlier.size = 2) + 
  # coord_flip() + 
  scale_fill_manual(name = '', values = c("#00AFBB", "#E7B800", "#FC4E07")) + 
  scale_color_manual(name = '', values = c("#00AFBB", "#E7B800", "#FC4E07")) + 
  facet_rep_wrap(.~SubExp, repeat.tick.labels = TRUE, scales = 'free') + 
  geom_text(data = medias, aes(label = media, y = media + 0.01),size =4) +
  xlab("Amostras") + 
  ylab("Resistência à compressão (kgf/pelota)") + 
  # ggtitle(paste(legenda[j,3],'das pelotas de granulometria',granulist[i])) + 
  # scale_color_manual(name = 'Família', values =  cores) + 
  # scale_fill_manual(name = 'Família', values =  cores) + 
  theme  
gres

tiff('outputdata/plots/pelletizacao/resistencia.tiff', units ='in', width = 12, height = 6, res = 200)
gres
dev.off()


