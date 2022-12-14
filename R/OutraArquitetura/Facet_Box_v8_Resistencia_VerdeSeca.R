setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos\\Pelotizacao\\Boxplot\\R')

# Bibliotecas -------------------------------------------------------------
library(reshape2) ##Biblioteca que cont?m o reshape para transformar de linhas para colunas
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas\\database\\database_no_dash.csv')
mydata = read.csv('Planilhas\\database\\database.csv', encoding = 'UTF-8')
names(mydata)[1] = 'Experimento'

# Criar nova coluna porque esqueci no Excel: (fun??o paste permite isso )
mydata$Amostras = paste(mydata$Min?rio,mydata$TS,sep = "-")

#Substituir os c?digos A e B pelos tamanhos das pelotas reais
mydata$A.B = sub("A","-16+12,5 mm",mydata$A.B)
mydata$A.B = sub("B","-12,5+9,5 mm",mydata$A.B)
#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)


# Plot --------------------------------------------------------------------
subres = subset(mydata, Experimento == 'Resist?ncia')
subresistencia = subset(subres, SubSubExp == 'Verde' | SubSubExp == 'Seca')

library(dplyr)
#Para poder agrupar por SubSubExp que cont?m a especifica??o de temperaturas do choque t?rmico
medias = subresistencia %>%
  group_by(A.B, SubSubExp, Amostras) %>%
  summarise_at(vars(Valor), list(name = mean))
names(medias)[4] = 'media'
medias[4] = round(medias[4], 1)

#Reorder for facet
neworder = c("Verde","Seca")
library(plyr)  ## or dplyr (transform -> mutate)
subresistencia <- arrange(transform(subresistencia,
                           SubSubExp=factor(SubSubExp,levels=neworder)),SubSubExp)

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

g = ggplot(subresistencia, aes(x = Amostras, y = Valor)) + 
  geom_boxplot(aes(color = Amostras), outlier.size = 0.8) + 
  facet_wrap(SubSubExp~A.B, scales = "free") + 
  geom_text_repel(data = medias, aes(label = media, y = media),size =5, direction = "y", force = 25) + 
  xlab("") + 
  ylab("Resist?ncia (kgf/pelota)") + 
  scale_color_manual(values = cores, 
                    breaks = levels(factor(subresistencia$Amostras))) + 
  theme 
  #stat_summary(fun=mean, geom="point", size=0.1, color="black", fill="black") +

tiff('imgs/facet/Resistencia_Verde_Seca.tiff', units = "in", width = 9.2, height = 9.2, res = 300)
g
dev.off()

