setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomera��o de grossos\\Reuni�es apresenta��es\\20200813_Interna_Dispersao_Granulometria\\Boxplot\\R')

library(reshape2)
library(tidyverse)

quedas1 = read.csv('data.csv', encoding = 'ISO-8859-1', header = T)
quedas1['index'] = c(1:1:nrow(quedas1))


lista = names(quedas1)
meltdata = melt(data = quedas1, 
                id.vars = c('index'), 
                measure.vars = lista)
names(meltdata)[2] = 'Amostra'
names(meltdata)[3] = 'Resist�ncias'


g = ggplot(meltdata, aes(x = Amostra, y = Resist�ncias)) + 
  geom_boxplot(aes(color = Amostra)) + 
  #facet_wrap(.~Amostra) +#scales = free? 
  theme(strip.text.x = element_blank()) + 
  ylab('Valores') + 
  xlab(' ')
g

