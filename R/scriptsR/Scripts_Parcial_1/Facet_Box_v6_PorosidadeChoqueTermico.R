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
#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)


# Plot --------------------------------------------------------------------
subres = subset(mydata, Experimento == 'Porosidade')
subresporo = subset(subres, SubExp == 'Choque T?rmico')

library(dplyr)
#Para poder agrupar por SubSubExp que cont?m a especifica??o de temperaturas do choque t?rmico
medias = subresporo %>%
  group_by(SubSubExp, Amostras) %>%
  summarise_at(vars(Valor), list(name = mean))
names(medias)[3] = 'media'
medias[3] = round(medias[3], 1)

#Reorder for facet
neworder = c("300 ?C","500 ?C","700 ?C","900 ?C",'1100 ?C')
library(plyr)  ## or dplyr (transform -> mutate)
subresporo <- arrange(transform(subresporo,
                           SubSubExp=factor(SubSubExp,levels=neworder)),SubSubExp)

theme = theme_bw(base_size = 15) + 
  theme(legend.position = c(0.8, 0.15),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 16), #Posi??o da legenda
        plot.title = element_text(hjust =0.5), #Posi??o do t?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill = NA,colour = NA)) #Cor do background dos t?tulos de cada face

cores = rep(c("#FF0000","#0000FF","#FFA500","#800080","#00FF00"),5)

g = ggplot(subresporo, aes(x = Amostras, y = Valor)) + 
  geom_boxplot(aes(color = Amostras), outlier.size = 0.8) + 
  facet_wrap(.~SubSubExp, scales = "free", nrow = 3, ncol = 2) + 
  geom_text(data = medias, aes(label = media, y = 1.02*media),size =5) +
  xlab("") + 
  ylab("Porosidade (%)") + 
  scale_color_manual(values = cores, 
                    breaks = levels(factor(subresporo$Amostras))) + 
  theme 
  #stat_summary(fun=mean, geom="point", size=0.1, color="black", fill="black") +

tiff('imgs/facet/Porosidade_ChoqueTermico.tiff', units = "in", width = 10, height = 10, res = 300)
g
dev.off()

