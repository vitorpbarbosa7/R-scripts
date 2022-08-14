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
#Remove the NAs returned (valores nulos que não existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)


# Plot --------------------------------------------------------------------
subres = subset(mydata, Experimento == 'Resistência')
subreschoq = subset(subres, SubExp == 'Choque Térmico')

library(dplyr)
#Para poder agrupar por SubSubExp que contém a especificação de temperaturas do choque térmico
medias = subreschoq %>%
  group_by(SubSubExp, Amostras) %>%
  summarise_at(vars(Valor), list(name = mean))
names(medias)[3] = 'media'
medias[3] = round(medias[3], 1)

#Reorder for facet
neworder = c("300 °C","500 °C","700 °C","900 °C",'1100 °C')
library(plyr)  ## or dplyr (transform -> mutate)
subreschoq <- arrange(transform(subreschoq,
                           SubSubExp=factor(SubSubExp,levels=neworder)),SubSubExp)

theme = theme_bw(base_size = 15) + 
  theme(legend.position = c(0.8, 0.15),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 16), #Posição da legenda
        plot.title = element_text(hjust =0.5), #Posição do título
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA,colour=NA)) #Cor do background dos títulos de cada face

cores = rep(c("#FF0000","#0000FF","#FFA500","#800080","#00FF00"),5)

g = ggplot(subreschoq, aes(x = Amostras, y = Valor)) + 
  geom_boxplot(aes(color = Amostras),outlier.size = 0.8) + 
  facet_wrap(.~SubSubExp, scales = "free", nrow = 3, ncol = 2) + 
  geom_text(data = medias, aes(label = media, y = 1.1*media),size =5) +
  xlab("") + 
  ylab("Resistências (kgf/pelota)") + 
  scale_color_manual(values = cores, 
                    breaks = levels(factor(subreschoq$Amostras))) + 
  theme 
  #stat_summary(fun=mean, geom="point", size=0.1, color="black", fill="black") +

tiff('imgs/facet/Resistencia_Choq_Ter.tiff', units = "in", width = 10, height = 10, res = 300)
g
dev.off()



# Outras formas de organizar os dados
ggplot(subreschoq, aes(x = SubSubExp, y = Valor)) + 
  geom_boxplot(aes(fill = Amostras)) + 
  facet_wrap(.~Amostras, scales = "free") + 
  stat_summary(fun=mean, geom="point", size=0.1, color="black", fill="black") +
  theme_bw()
