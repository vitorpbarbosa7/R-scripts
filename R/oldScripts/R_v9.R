setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos\\Reuni?es apresenta??es\\20200813_Interna_Dispersao_Granulometria\\Boxplot\\R')

library(reshape2)
library(tidyverse)

ggbox = function(pathin, variavel,titulo, ylab){

#ggbox = function(pathin){
###Come?o do Script da fun??o  
quedas1 = read_csv(pathin)

lista = names(quedas1)
meltdata = melt(data = quedas1, 
                #id.vars = c('index'), 
                measure.vars = lista)
names(meltdata)[1] = 'Amostra'
names(meltdata)[2] = variavel

medias = aggregate(.~Amostra,data=meltdata,mean)
names(medias)[2] = 'media'

g = ggplot(meltdata, aes(x = Amostra, y = meltdata[,2])) + 
  geom_boxplot(aes(color = Amostra)) + 
  #facet_wrap(.~Amostra) +#scales = free? 
  theme(legend.position = 'none',
        plot.title = element_text(hjust =0.5)) + 
  geom_text(data = medias, aes(label = media, y = media  + 0.15)) + 
  stat_summary(fun=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  ylab(ylab) + 
  xlab(' ') + 
  ggtitle(titulo)

pathout = paste('imgs//',titulo,'.tiff')

return(g)
}
#tiff(pathout, width = w, height = h, res = res)

w = 1800
h = 1000
res = 150
# Quedas 1 ----------------------------------------------------------------
tiff('imgs//Quedas -16+12,5mm.tiff', width = w, height = h, res = res)
ggbox('csvs\\quedas1.csv', 'Quedas','Quedas -16+12,5mm', 'N? de Quedas')
dev.off()

# Densidade Aparente A ----------------------------------------------------
tiff('imgs\\Densidade Aparente -16+12,5mm.tiff', width = w, height = h, res = res)
ggbox('csvs\\DensApaA.csv', 'Densidade Aparente','Densidade Aparente -16+12,5mm', 'Densidade Aparente (g/cm?)')
dev.off()

