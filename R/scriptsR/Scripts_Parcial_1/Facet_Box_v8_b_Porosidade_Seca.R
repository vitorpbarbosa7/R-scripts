setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos\\Pelotizacao\\Boxplot\\R')

# Bibliotecas -------------------------------------------------------------
library(reshape2) ##Biblioteca que cont?m o reshape para transformar de linhas para colunas
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas\\database\\database_no_dash.csv')
mydata = read.csv('Planilhas\\database\\databasenewSubExp.csv', encoding = 'UTF-8')
names(mydata)[1] = 'Experimento'

# Criar nova coluna porque esqueci no Excel: (fun??o paste permite isso )
mydata$Amostras = paste(mydata$Min?rio,mydata$TS,sep = "-")

#Substituir os c?digos A e B pelos tamanhos das pelotas reais
mydata$A.B = sub("A","-16+12,5 mm",mydata$A.B)
mydata$A.B = sub("B","-12,5+9,5 mm",mydata$A.B)
#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)

# Plot --------------------------------------------------------------------
subdata = subset(mydata, (Experimento == 'Porosidade' &
                   SubExp == 'Seca'))

#Reorder for facet
neworder = c("-16+12,5 mm","-12,5+9,5 mm")
library(plyr)  ## or dplyr (transform -> mutate)
subdata <- arrange(transform(subdata,
                             A.B=factor(A.B,levels=neworder)),A.B)

library(dplyr)
#Para poder agrupar por SubSubExp que cont?m a especifica??o de temperaturas do choque t?rmico
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
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust =0.5), #Posi??o do t?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

cores = rep(c("#FF0000","#0000FF","#FFA500","#800080","#00FF00"),5)

g = ggplot(subdata, aes(x = Amostras, y = Valor)) + 
  geom_boxplot(aes(color = Amostras), outlier.size = 0.8) + 
  facet_wrap(.~A.B, scales = "free", nrow = 2, ncol = 1) + 
  geom_text(data = medias, aes(label = media, y = media + 0.01),size =5) +
  xlab("") +
  ylab("Porosidade (%)") + 
  #scale_y_continuous(name = "Resist?ncias (kgf/pelota)") + 
  scale_color_manual(values = cores, 
                    breaks = levels(factor(subdata$Amostras))) +
  theme  
  #ggtitle("Resist?ncia queimada da pelota de PFM")
  #stat_summary(fun=mean, geom="point", size=0.1, color="black", fill="black") +
g

tiff('imgs/facet/Poros_Seca.tiff', width = 1200, height = 1000, res = 120)
g
dev.off()

