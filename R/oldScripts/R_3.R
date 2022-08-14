setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomeração de grossos\\Reuniões apresentações\\20200813_Interna_Dispersao_Granulometria\\Boxplot\\R')

library(reshape2)
library(tidyverse)

quedas1 = read_csv('quedas1.csv')

lista = names(quedas1)
meltdata = melt(data = quedas1, 
                #id.vars = c('index'), 
                measure.vars = lista)
names(meltdata)[1] = 'Amostra'
names(meltdata)[2] = 'Resistências'

medias = aggregate(.~Amostra,data=meltdata,mean)
names(medias)[2] = 'media'

g = ggplot(meltdata, aes(x = Amostra, y = Resistências)) + 
  geom_boxplot(aes(color = Amostra)) + 
  #facet_wrap(.~Amostra) +#scales = free? 
  theme_bw() + 
  geom_text(data = medias, aes(label = media, y = media  + 0.15)) + 
  stat_summary(fun=mean, geom="point", shape=4, size=3, color="black", fill="black") +
  ylab('Valores') + 
  xlab(' ') + 
  ggtitle('Resistências da pelota ')

tiff('imgs//quedas1.tiff', width = 2000, height = 600, res = 150)
g
dev.off()


