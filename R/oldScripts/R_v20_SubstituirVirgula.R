setwd('C:\\Users\\vpb\\IPT\\Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos\\Pelotizacao\\Boxplot\\R')


# Bibliotecas -------------------------------------------------------------
library(reshape2) ##Biblioteca que cont?m o reshape para transformar de linhas para colunas
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(forestmangr) #Biblioteca com o round_df
library(swfscMisc) #Biblioteca com outro round trunc(x, ...)

# Par?metros dos gr?ficos -------------------------------------------------
# Defini??o de A e B ------------------------------------------------------
A = '-16+12,5 mm'
B = '-12,5+9,5 mm'
img = 'imgs\\'
# Defini??es gr?fico ------------------------------------------------------
w = 900
h = 600
res = 100

# Unidades ----------------------------------------------------------------
kgf = '(kgf/pelota)'

#Fun??o que realizar? o tratamento e o plot
ggbox = function(pathin, variavel,titulo, ylab,unit){

      #ggbox = function(pathin){
      ###Come?o do Script da fun??o  2
      df = read_csv(pathin)
      df = Filter(function(x) !(all(x=="")), df) #Remover qualquer coluna que tenha valores nulos
      
      #Tratar os nomes para retirar os milimetros a direita
      fun_sub = function(x){
        return(substr(x,1,nchar(x)-12))
      }
      
      names(df) = sapply(names(df), fun_sub)
      
      #Tratar os nomes
      lista = names(df)
      lista = gsub("Pellet Feed","PFM",lista)
      lista = gsub("Pellet feed","PFM",lista)
      lista = gsub("CG","PFNM",lista)
      names(df) = lista
      
      #Transforma??o de linhas para
      meltdata = melt(data = df, 
                      #id.vars = c('index'), 
                      measure.vars = lista)
      names(meltdata)[1] = 'Amostra'
      names(meltdata)[2] = variavel
      
      # Fun??o que calculava a m?dia quando era inserida atrav?s de geom_text e n?o stats_summary
      medias = aggregate(.~Amostra,data=meltdata,mean)
      medias = round(medias, 2)
      names(medias)[2] = 'media'
      
      #Fun??o que permitir? calcular a m?dia dentro do ggplot
      fun_mean <- function(x){
        return(data.frame(y=mean(x),label=mean(x,na.rm=T)))}
      
      #Tema do gr?fico
      theme = theme_bw(base_size = 15) + 
        theme(legend.position = 'none', #Sem legenda
              plot.title = element_text(hjust =0.5), #Posi??o do t?tulo
              panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
              axis.text.x=element_text(colour="black"),
              axis.text.y=element_text(colour="black")) #Cor do texto dos eixos
      
      g = ggplot(meltdata, aes(x = Amostra, y = meltdata[,2])) + 
        geom_boxplot(aes(fill = Amostra)) + 
        #facet_wrap(.~Amostra) +#scales = free? 
        # theme(legend.position = 'none',
        #       plot.title = element_text(hjust =0.5)) + 
        geom_text(data = medias, aes(label = media, y = media + 0.01)) +
        #Poss?vel deslocar fazendo media + 0.01 por ex.
        #stat_summary(fun=mean, geom="point", size=0.5, color="black", fill="black") +
        #stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) + 
        ylab(paste(ylab,unit)) + 
        xlab(' ') + 
        ggtitle(titulo) + 
        theme
      
      pathout = paste('imgs//',titulo,'.tiff')
      
      return(g)
}
# Quedas 1 ----------------------------------------------------------------
tiff('imgs\\Quedas -16+12,5mm.tiff', width = w, height = h, res = res)
ggbox('csvs\\quedas1.csv', 'Quedas','Quedas -16+12,5mm', 'N? de Quedas','(-)')
dev.off()

# Densidades --------------------------------------------------------------
# Densidade Aparente Verde A ----------------------------------------------------
tiff(paste(img,'Densidade Aparente Verde',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaA.csv', 'Densidade Aparente Verde',paste('Densidade Aparente Verde',A), 'Densidade Aparente','(g/cm?)')
dev.off()

# Densidade Aparente Verde B----------------------------------------------------
tiff(paste(img,'Densidade Aparente Verde',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaB.csv', 'Densidade Aparente Verde',paste('Densidade Aparente Verde',B), 'Densidade Aparente','(g/cm?)')
dev.off()

# Densidade Aparente Seca A -----------------------------------------------
tiff(paste(img,'Densidade Aparente Seca',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaSecaA.csv', 'Densidade Aparente Seca',paste('Densidade Aparente Seca',A), 'Densidade Aparente','(g/cm?)')
dev.off()

# Densidade Aparente Seca B -----------------------------------------------
tiff(paste(img,'Densidade Aparente Seca',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaSecaB.csv', 'Densidade Aparente Seca',paste('Densidade Aparente Seca',B), 'Densidade Aparente','(g/cm?)')
dev.off()

# Densidade Aparente Queimada A -----------------------------------------------
tiff(paste(img,'Densidade Aparente Queimada',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaQueiA.csv', 'Densidade Aparente Queimada',
      paste('Densidade Aparente Queimada',A), 'Densidade Aparente','(g/cm?)')
dev.off()

# Densidade Aparente Queimada B -----------------------------------------------
tiff(paste(img,'Densidade Aparente Queimada',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaQueiB.csv', 'Densidade Aparente Queimada',
      paste('Densidade Aparente Queimada',B), 'Densidade Aparente','(g/cm?)')
dev.off()


# Densidade Aparente Choque T?rmico 300 B-----------------------------------
tiff(paste(img,'Densidade Aparente Choque Termico 300',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaChoqTer300.csv','Densidade Aparente Choque Termico',
      paste('Densidade Aparente Choque T?rmico 300 ?C',B), 'Densidade Aparente','(g/cm?)')
dev.off()

# Densidade Aparente Choque T?rmico 500 B-----------------------------------
tiff(paste(img,'Densidade Aparente Choque Termico 500',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaChoqTer500.csv','Densidade Aparente Choque Termico',
      paste('Densidade Aparente Choque T?rmico 500 ?C',B), 'Densidade Aparente','(g/cm?)')
dev.off()

# Densidade Aparente Choque T?rmico 700 B-----------------------------------
tiff(paste(img,'Densidade Aparente Choque Termico 700',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaChoqTer700.csv','Densidade Aparente Choque Termico',
      paste('Densidade Aparente Choque T?rmico 700 ?C',B), 'Densidade Aparente','(g/cm?)')
dev.off()

# Densidade Aparente Choque T?rmico 900 B-----------------------------------
tiff(paste(img,'Densidade Aparente Choque Termico 900',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaChoqTer900.csv','Densidade Aparente Choque Termico',
      paste('Densidade Aparente Choque T?rmico 900 ?C',B), 'Densidade Aparente','(g/cm?)')
dev.off()

# Densidade Aparente Choque T?rmico 1100 B-----------------------------------
tiff(paste(img,'Densidade Aparente Choque Termico 1100',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\DensApaChoqTer1100.csv','Densidade Aparente Choque Termico',
      paste('Densidade Aparente Choque T?rmico 1100 ?C',B), 'Densidade Aparente','(g/cm?)')
dev.off()


# Porosidades  ------------------------------------------------------------
# Porosidade  Verde A ------------------------------------------------------------
tiff(paste(img,'Porosidade Verde',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorVerdeA.csv', 'Porosidade Verde',paste('Porosidade',A), 'Porosidade ','(-)')
dev.off()

# Porosidade Aparante Verde B ------------------------------------------------------------
tiff(paste(img,'Porosidade Verde',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorVerdeB.csv', 'Porosidade Verde',paste('Porosidade',B), 'Porosidade ','(-)')
dev.off()

# Porosidade  Seca A -------------------------------------------------------
tiff(paste(img,'Porosidade Seca',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorSecaA.csv', 'Porosidade Seca',paste('Porosidade',A), 'Porosidade ','(-)')
dev.off()

# Porosidade  Seca B -------------------------------------------------------
tiff(paste(img,'Porosidade  Seca',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorSecaB.csv', 'Porosidade Seca',paste('Porosidade',B), 'Porosidade Seca','(-)')
dev.off()

# Porosidade  Queimada A -------------------------------------------------------
tiff(paste(img,'Porosidade Queimada',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorQueiA.csv', 'Porosidade Queimada',paste('Porosidade',A), 'Porosidade ','(-)')
dev.off()

# Porosidade  Queimada B -------------------------------------------------------
tiff(paste(img,'Porosidade  Queimada',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorQueiB.csv', 'Porosidade Queimada',paste('Porosidade',B), 'Porosidade ','(-)')
dev.off()


# Porosidade  Choque T?rmico 300 B-----------------------------------
tiff(paste(img,'Porosidade Choque Termico 300',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorApaChoqTer300.csv','Porosidade Choque Termico',
      paste('Porosidade Choque T?rmico 300 ?C',B), 'Porosidade ','(g/cm?)')
dev.off()

# Porosidade  Choque T?rmico 500 B-----------------------------------
tiff(paste(img,'Porosidade  Choque Termico 500',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorApaChoqTer500.csv','Porosidade  Choque Termico',
      paste('Porosidade Choque T?rmico 500 ?C',B), 'Porosidade','(g/cm?)')
dev.off()

# Porosidade  Choque T?rmico 700 B-----------------------------------
tiff(paste(img,'Porosidade  Choque Termico 700',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorApaChoqTer700.csv','Porosidade  Choque Termico',
      paste('Porosidade  Choque T?rmico 700 ?C',B), 'Porosidade ','(g/cm?)')
dev.off()

# Porosidade  Choque T?rmico 900 B-----------------------------------
tiff(paste(img,'Porosidade  Choque Termico 900',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorApaChoqTer900.csv','Porosidade  Choque Termico',
      paste('Porosidade Choque T?rmico 900 ?C',B), 'Porosidade ','(g/cm?)')
dev.off()

# Porosidade  Choque T?rmico 1100 B-----------------------------------
tiff(paste(img,'Porosidade  Choque Termico 1100',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\PorApaChoqTer1100.csv','Porosidade  Choque Termico',
      paste('Porosidade Choque T?rmico 1100 ?C',B), 'Porosidade','(g/cm?)')
dev.off()

# Resist?ncias ------------------------------------------------------------
# Resist?ncia Choque T?rmico 300 ?C -------------------------------------
tiff(paste(img,'Resist?ncia Choque T?rmico 300 ?C',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisChoqTer300B.csv', 'Resist?ncia Choque T?rmico',
      paste('Resist?ncia Choque T?rmico 300 ?C',B),'Resist?ncia',kgf)
dev.off()

# Resist?ncia Choque T?rmico 500 ?C -------------------------------------
tiff(paste(img,'Resist?ncia Choque T?rmico 500 ?C',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisChoqTer500B.csv', 'Resist?ncia Choque T?rmico',
      paste('Resist?ncia Choque T?rmico 500 ?C',B),'Resist?ncia',kgf)
dev.off()

# Resist?ncia Choque T?rmico 700 ?C -------------------------------------
tiff(paste(img,'Resist?ncia Choque T?rmico 700 ?C',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisChoqTer700B.csv', 'Resist?ncia Choque T?rmico',
      paste('Resist?ncia Choque T?rmico 700 ?C',B),'Resist?ncia',kgf)
dev.off()

# Resist?ncia Choque T?rmico 900 ?C -------------------------------------
tiff(paste(img,'Resist?ncia Choque T?rmico 900 ?C',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisChoqTer900B.csv', 'Resist?ncia Choque T?rmico',
      paste('Resist?ncia Choque T?rmico 900 ?C',B),'Resist?ncia',kgf)
dev.off()

# Resist?ncia Choque T?rmico 1100 ?C -------------------------------------
tiff(paste(img,'Resist?ncia Choque T?rmico 1100 ?C',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisChoqTer1100B.csv', 'Resist?ncia Choque T?rmico',
      paste('Resist?ncia Choque T?rmico 1100 ?C',B),'Resist?ncia',kgf)
dev.off()

# Resist?ncia Queimada A --------------------------------------------------
tiff(paste(img,'Resist?ncia Queimada',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisQueiA.csv', 'Resist?ncia Queimada',paste('Resist?ncia Queimada',A), 'Resist?ncia',kgf)
dev.off()

# Resist?ncia Queimada B --------------------------------------------------
tiff(paste(img,'Resist?ncia Queimada',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisQueiB.csv', 'Resist?ncia Queimada',paste('Resist?ncia Queimada',B), 'Resist?ncia',kgf)
dev.off()

# Resist?ncia Seca A --------------------------------------------------
tiff(paste(img,'Resist?ncia seca',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisSecaA.csv', 'Resist?ncia seca',paste('Resist?ncia seca',A), 'Resist?ncia',kgf)
dev.off()

# Resist?ncia Seca B --------------------------------------------------
tiff(paste(img,'Resist?ncia seca',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisSecaB.csv', 'Resist?ncia seca',paste('Resist?ncia seca',B), 'Resist?ncia',kgf)
dev.off()

# Resist?ncia Verde A --------------------------------------------------
tiff(paste(img,'Resist?ncia verde',A,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisVerdeA.csv', 'Resist?ncia verde',paste('Resist?ncia verde',A), 'Resist?ncia',kgf)
dev.off()

# Resist?ncia Verde B --------------------------------------------------
tiff(paste(img,'Resist?ncia verde',B,'.tiff',sep=""), width = w, height = h, res = res)
ggbox('csvs\\ResisVerdeB.csv', 'Resist?ncia verde',paste('Resist?ncia verde',B), 'Resist?ncia',kgf)
dev.off()


