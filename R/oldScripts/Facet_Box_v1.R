setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos\\Pelotizacao\\Boxplot\\R')


# Bibliotecas -------------------------------------------------------------
library(reshape2) ##Biblioteca que cont?m o reshape para transformar de linhas para colunas
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv


# Importacao dos dados ----------------------------------------------------
mydata = read_csv('Planilhas\\database\\database.csv')

# Criar nova coluna porque esqueci no Excel: (fun??o paste permite isso )
mydata$Iron_and_size = paste(mydata$Min?rio,mydata$TS,sep = "-")




# Plot --------------------------------------------------------------------

subres = subset(mydata, Experimento == 'Resist?ncia')

subreschoq = subset(subres, SubExp == 'Choque T?rmico')

library(dplyr)
medias = subreschoq %>%
  group_by(SubSubExp, Iron_and_size) %>%
  summarise_at(vars(Valor), list(name = mean))
names(medias)[3] = 'media'
medias[3] = round(medias[3], 1)

#Reorder for facet
neworder = c("300 ?C","500 ?C","700 ?C","900 ?C",'1100 ?C')
library(plyr)  ## or dplyr (transform -> mutate)
subreschoq <- arrange(transform(subreschoq,
                           SubSubExp=factor(SubSubExp,levels=neworder)),SubSubExp)

ggplot(subreschoq, aes(x = Iron_and_size, y = Valor)) + 
  geom_boxplot(aes(fill = Iron_and_size)) + 
  facet_wrap(.~SubSubExp, scales = "free") + 
  geom_text(data = medias, aes(label = media, y = media + 0.01),size =2) +
  theme_bw()
  #stat_summary(fun=mean, geom="point", size=0.1, color="black", fill="black") +
  
#
ggplot(subreschoq, aes(x = SubSubExp, y = Valor)) + 
  geom_boxplot(aes(fill = Iron_and_size)) + 
  facet_wrap(.~Iron_and_size, scales = "free") + 
  stat_summary(fun=mean, geom="point", size=0.1, color="black", fill="black") +
  theme_bw()
