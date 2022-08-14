setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
rm(list=ls())
df_A = read.csv('outputdata/df_plot.csv')

# install.packages("lemon")
library(tidyverse)
library(lemon)
# Transformação de fatores e numeric
df_A[names(df_A[c(1:13)])] = lapply(df_A[names(df_A[c(1:13)])], factor)

df_A[names(df_A[-c(1:13)])] = sapply(df_A[names(df_A[-c(1:13)])], as.numeric)

str(df_A)
size = 5
# Para plot
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do t?tulo
        axis.line=element_line(), #Adicionar os eixos em todas facetas
        #panel.border = element_blank(), panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # axis.text.x=element_text(colour="black", angle = 75, hjust = 1, size = 8),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

#Criar looping para plot 

variaveis = c(14,16,18,20,22,24,26,28)
varnamesa = names(df_A[variaveis])
varnamesa

#Para legendas
axisnames = c('Resistência (kgf/pelota)','Resistência (kgf/pelota)','N° de Quedas (-)', 
              'Dispersão (%)','D10 (µm)','D50 (µm)','D90 (µm)','Resistência (kgf/pelota)')
titulos = c('Resistência Verde','Resistência Seca','N° de Quedas', 
            'Dispersão','D10','D50','D90','Resistência Queimada')
legenda = data.frame(varnamesa,axisnames, titulos)
legenda


# Plots -------------------------------------------------------------------
glist1 = list()
index = 0
# limites do looping
init = 3
end = 4
# Todos em função da Dispersão
for (j in init:end){
  for (i in init:end) {
    if (i != j){
      index = index + 1
      glist1[[index]] = ggplot(df_A, aes_string(x = varnamesa[j], y = varnamesa[i])) + 
        geom_point(mapping = aes(fill = Aditivo1,  colour = Aditivo1, shape = ID), size = size) + 
        scale_shape_manual(values=1:nlevels(df_A$ID)) + 
        facet_rep_wrap(.~Familia1, repeat.tick.labels = TRUE) + 
        theme + 
        # scale_x_continuous(limits=c(0,max(df_A[,varnamesa[j]]))) + # Selecionar max de acordo com a coluna
        # scale_y_continuous(limits=c(0,max(df_A[,varnamesa[i]]))) + 
        xlab(legenda[j,2]) + 
        ylab(legenda[i,2]) + 
        ggtitle(paste(legenda[i,3],' X ',legenda[j,3], paste =''))
      print(varnamesa[j])
    }
  }
}
path = 'C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots'
path
index = 0; w = 12; h = 8; res = 80;
for (j in init:end){
  for (i in init:end){
    if (i != j){
      index = index + 1
      print(index)
      mypath = file.path(path,paste('Familia1/',varnamesa[j],'_VS_',varnamesa[i]), sep = '')
      filename = paste(mypath, '.tiff', sep = '')
      tiff(filename, units = 'in', width = w, height = h, res = res)
      print(glist1[[index]])
      dev.off()
    }
  }
}

# Familia2 ----------------------------------------------------------------x  
glist2 = list()
4index = 0
# Todos em função da Dispersão
for (j in init:end){
  for (i in init:end) {
    if (i != j){
      index = index + 1
      glist2[[index]] = ggplot(df_A, aes_string(x = varnamesa[j], y = varnamesa[i])) + 
        geom_point(mapping = aes(fill = Aditivo1,  colour = Aditivo1, shape = ID), size = size) + 
        scale_shape_manual(values=1:nlevels(df_A$ID)) + 
        facet_rep_wrap(.~Familia2, repeat.tick.labels = TRUE) + 
        theme + 
        # scale_x_continuous(limits=c(0,max(df_A[,varnamesa[j]]))) + # Selecionar max de acordo com a coluna
        # scale_y_continuous(limits=c(0,max(df_A[,varnamesa[i]]))) + 
        xlab(legenda[j,2]) + 
        ylab(legenda[i,2]) + 
        ggtitle(paste(legenda[i,3],' X ',legenda[j,3], paste =''))
    }
  }
}

path = 'C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots'
path
index = 0
for (j in init:end){
  for (i in init:end){
    if (i != j){
      index = index + 1
      print(index)
      mypath = file.path(path,paste('Familia2/',varnamesa[j],'_VS_',varnamesa[i]), sep = '')
      filename = paste(mypath, '.tiff', sep = '')
      tiff(filename, units = 'in', width = w, height = h, res = res)
      print(glist2[[index]])
      dev.off()
    }
  }
}

