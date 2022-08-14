setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomeração de grossos\\Pelotizacao\\Boxplot\\R')

# Bibliotecas -------------------------------------------------------------
library(reshape2) ##Biblioteca que contém o reshape para transformar de linhas para colunas
library(tidyverse) #Biblioteca que contém o poderoso ggplot e read_csv e não o raw built-in do R read.csv

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas\\database\\database_no_dash.csv')
mydata = read.csv('Planilhas\\database\\databasenewSubExp.csv', encoding = 'UTF-8')
names(mydata)[1] = 'Experimento'

# Criar nova coluna porque esqueci no Excel: (função paste permite isso )
mydata$Amostras = paste(mydata$Minério,mydata$TS,sep = "-")

#Substituir os códigos A e B pelos tamanhos das pelotas reais
mydata$A.B = sub("A","-16+12,5 mm",mydata$A.B)
mydata$A.B = sub("B","-12,5+9,5 mm",mydata$A.B)
#Remove the NAs returned (valores nulos que não existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)

# Plot --------------------------------------------------------------------
subdata = subset(mydata, (Experimento == 'Resistência' &
                   SubExp == 'Seca'))

#Reorder for facet
neworder = c("-16+12,5 mm","-12,5+9,5 mm")
library(plyr)  ## or dplyr (transform -> mutate)
subdata <- arrange(transform(subdata,
                             A.B=factor(A.B,levels=neworder)),A.B)

library(dplyr)
#Para poder agrupar por SubSubExp que contém a especificação de temperaturas do choque térmico
medias = subdata %>%
  group_by(A.B, Amostras) %>%
  summarise_at(vars(Valor), list(name = mean))
names(medias)[3] = 'media'
medias[3] = round(medias[3], 1)

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

cores = rep(c("#FF0000","#0000FF","#FFA500","#800080","#00FF00"),5)

g = ggplot(subdata, aes(x = Amostras, y = Valor)) + 
  geom_boxplot(aes(color = Amostras), outlier.size = 0.8) + 
  facet_wrap(.~A.B, scales = "free", nrow = 2, ncol = 1) + 
  geom_text(data = medias, aes(label = media, y = media + 0.01),size =5) +
  xlab("") +
  ylab("Resistência (kgf/pelota)") + 
  #scale_y_continuous(name = "Resistências (kgf/pelota)") + 
  scale_color_manual(values = cores, 
                    breaks = levels(factor(subdata$Amostras))) +
  theme  
  #ggtitle("Resistência queimada da pelota de PFM")
  #stat_summary(fun=mean, geom="point", size=0.1, color="black", fill="black") +
g

tiff('imgs/facet/Resis_Seca.tiff', width = 1200, height = 1000, res = 120)
g
dev.off()

