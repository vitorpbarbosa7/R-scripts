setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos\\Reuni?es apresenta??es\\20200813_Interna_Dispersao_Granulometria\\Boxplot\\R')

library(reshape2)
library(tidyverse)

ggbox = function(pathin, variavel,titulo, ylab){

#ggbox = function(pathin){
###Come?o do Script da fun??o  
df = read_csv(pathin)
df = Filter(function(x) !(all(x=="")), df) #Remover qualquer coluna que tenha valores nulos

lista = names(df)
meltdata = melt(data = df, 
                #id.vars = c('index'), 
                measure.vars = lista)
names(meltdata)[1] = 'Amostra'
names(meltdata)[2] = variavel

# Arredondar todos os dados para 2 casas decimais:
meltdata[,2] = round(meltdata[,2],3)

# Fun??o que calculava a m?dia quando era inserida atrav?s de geom_text e n?o stats_summary
# medias = aggregate(.~Amostra,data=meltdata,mean)
# names(medias)[2] = 'media'

#Fun??o que permitir? calcular a m?dia dentro do ggplot
fun_mean <- function(x){
  return(data.frame(y=mean(x),label=mean(x,na.rm=T)))}

g = ggplot(meltdata, aes(x = Amostra, y = meltdata[,2])) + 
  geom_boxplot(aes(color = Amostra)) + 
  #facet_wrap(.~Amostra) +#scales = free? 
  theme(legend.position = 'none',
        plot.title = element_text(hjust =0.5)) + 
  #geom_text(data = medias, aes(label = media, y = media)) + #Poss?vel deslocar fazendo media + 0.01 por ex.
  stat_summary(fun=mean, geom="point", shape=4, size=2, color="black", fill="black") +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) + 
  ylab(ylab) + 
  xlab(' ') + 
  ggtitle(titulo)

pathout = paste('imgs//',titulo,'.tiff')

return(g)
}
#tiff(pathout, width = w, height = h, res = res)


# Defini??o de A e B ------------------------------------------------------
A = '-16+12,5 mm'
B = '-12,5+9,5 mm'
img = 'imgs\\'
# Defini??es gr?fico ------------------------------------------------------
w = 1800
h = 1000
res = 150
# Quedas 1 ----------------------------------------------------------------
tiff('imgs\\Quedas -16+12,5mm.tiff', width = w, height = h, res = res)
ggbox('csvs\\quedas1.csv', 'Quedas','Quedas -16+12,5mm', 'N? de Quedas')
dev.off()

# Densidade Aparente A ----------------------------------------------------
tiff(paste(img,'Densidade Aparente',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaA.csv', 'Densidade Aparente',paste('Densidade Aparente',A), 'Densidade Aparente (g/cm?)')
dev.off()

# Densidade Aparente B----------------------------------------------------
tiff(paste(img,'Densidade Aparente',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaB.csv', 'Densidade Aparente',paste('Densidade Aparente',B), 'Densidade Aparente (g/cm?)')
dev.off()

# Densidade Aparente Seca A -----------------------------------------------
tiff(paste(img,'Densidade Aparente Seca',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaSecaA.csv', 'Densidade Aparente Seca',paste('Densidade Aparente Seca',A), 'Densidade Aparente Seca (g/cm?)')
dev.off()

# Densidade Aparente Seca B -----------------------------------------------
tiff(paste(img,'Densidade Aparente Seca',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaSecaB.csv', 'Densidade Aparente Seca',paste('Densidade Aparente Seca',B), 'Densidade Aparente Seca (g/cm?)')
dev.off()

# Porosidade A ------------------------------------------------------------
tiff(paste(img,'Porosidade',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorA.csv', 'Porosidade',paste('Porosidade',A), 'Porosidade')
dev.off()

# Porosidade B ------------------------------------------------------------
tiff(paste(img,'Porosidade',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorB.csv', 'Porosidade',paste('Porosidade',B), 'Porosidade')
dev.off()

# Porosidade Seca A -------------------------------------------------------
tiff(paste(img,'Porosidade Seca',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorSecaA.csv', 'Porosidade Seca',paste('Porosidade Seca',A), 'Porosidade Seca')
dev.off()

# Porosidade Seca B -------------------------------------------------------
tiff(paste(img,'Porosidade Seca',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorSecaB.csv', 'Porosidade Seca',paste('Porosidade Seca',B), 'Porosidade Seca')
dev.off()

# Resist?ncia Choque T?rmico 300 ?C -------------------------------------
tiff(paste(img,'Resist?ncia Choque T?rmico 300 ?C','.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisChoqTer300.csv', 'Resist?ncia Choque T?rmico','Resist?ncia Choque T?rmico 300 ?C','Resist?ncia Choque T?rmico 300 ?C')
dev.off()
