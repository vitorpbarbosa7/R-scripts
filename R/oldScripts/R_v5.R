setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomeração de grossos\\Reuniões apresentações\\20200813_Interna_Dispersao_Granulometria\\Boxplot\\R')

library(reshape2)
library(tidyverse)

res = 150
h = 800
w = 1000
pathout = 'imgs//quedas1.tiff'
titulo = 'Quedas -16+12mm'
variavel = 'Resistências'
pathin = 'quedas1.csv'
ggbox = function(pathin, variavel,titulo,pathout,w,h,res){
  
  dados = read_csv(pathin)
  
  meltdata = melt(data = dados, 
                  #id.vars = c('index'), 
                  measure.vars = names(dados))
  names(meltdata)[1] = 'Amostra'
  names(meltdata)[2] = variavel
  
  medias = aggregate(.~Amostra,data=meltdata,mean)
  
  g = ggplot(meltdata, aes(x = Amostra, y = variavel)) + 
    geom_boxplot(aes(color = Amostra)) + 
    #facet_wrap(.~Amostra) +#scales = free? 
    theme_bw() + 
    geom_text(data = medias, aes(label = variavel, y = Resistências + 0.15)) + 
    stat_summary(fun=mean, geom="point", shape=4, size=3, color="black", fill="black") +
    ylab('Valores') + 
    xlab(' ') + 
    ggtitle(titulo) 
  
  tiff(pathout, width = w, height = h, res = res)
  g
  dev.off()
  }

ggbox('quedas1.csv','Resistências','Quedas -16+12mm','imgs//quedas1.tiff',w,h,res)

