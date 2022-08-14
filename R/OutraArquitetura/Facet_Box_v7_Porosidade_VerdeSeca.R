setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomeração de grossos\\Pelotizacao\\Boxplot\\R')

# Bibliotecas -------------------------------------------------------------
library(reshape2) ##Biblioteca que contém o reshape para transformar de linhas para colunas
library(tidyverse) #Biblioteca que contém o poderoso ggplot e read_csv e não o raw built-in do R read.csv

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas\\database\\database_no_dash.csv')
mydata = read.csv('Planilhas\\database\\database.csv', encoding = 'UTF-8')
names(mydata)[1] = 'Experimento'

# Criar nova coluna porque esqueci no Excel: (função paste permite isso )
mydata$Amostras = paste(mydata$Minério,mydata$TS,sep = "-")

#Substituir os códigos A e B pelos tamanhos das pelotas reais
mydata$A.B = sub("A","-16+12,5 mm",mydata$A.B)
mydata$A.B = sub("B","-12,5+9,5 mm",mydata$A.B)
#Remove the NAs returned (valores nulos que não existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)


# Plot --------------------------------------------------------------------
subres = subset(mydata, Experimento == 'Porosidade')
subresporo = subset(subres, SubExp == 'Verde' | SubExp == 'Seca')

library(dplyr)
#Para poder agrupar por SubSubExp que contém a especificação de temperaturas do choque térmico
medias = subresporo %>%
  group_by(A.B, SubExp, Amostras) %>%
  summarise_at(vars(Valor), list(name = mean))
names(medias)[4] = 'media'
medias[4] = round(medias[4], 1)

#Reorder for facet
neworder = c("Verde","Seca")
library(plyr)  ## or dplyr (transform -> mutate)
subresporo <- arrange(transform(subresporo,
                           SubExp=factor(SubExp,levels=neworder)),SubExp)

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

library(ggrepel)
g = ggplot(subresporo, aes(x = Amostras, y = Valor)) + 
  geom_boxplot(aes(color = Amostras), outlier.size = 0.8) + 
  facet_wrap(SubExp~A.B, scales = "free") + 
  #geom_text() +
  geom_text_repel(data = medias, aes(label = media, y = media),size =5, direction = "y", force = 15) + 
  xlab("") + 
  ylab("Porosidade (-)") + 
  scale_color_manual(values = cores, 
                    breaks = levels(factor(subresporo$Amostras))) + 
  theme 
  #stat_summary(fun=mean, geom="point", size=0.1, color="black", fill="black") +

tiff('imgs/facet/Porosidade_Verde_Seca.tiff', units = "in", width = 9.2, height = 9.2, res = 300)
g
dev.off()

