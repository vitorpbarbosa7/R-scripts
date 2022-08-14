setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
rm(list=ls())
df = read.csv('outputdata/df_plot_AB_TESTE.csv')

# install.packages("lemon")
library(tidyverse)x
library(lemon)
library(dplyr)
# Transformação de fatores e numeric
str(df)

df[names(df[c(1:13)])] = lapply(df[names(df[c(1:13)])], factor)

df[names(df[-c(1:13)])] = sapply(df[names(df[-c(1:13)])], as.numeric)

df$Amostra = as.factor(paste(df$Minerio,df$TS,
                                 df$Dose1,df$Aditivo1,
                                 df$Dose2,df$Aditivo2,
                                 df$Dose3,df$Aditivo3))
str(df)
# Mudar ordem de fatores --------------------------------------------------
#  Ordem das amostras para apresentação no boxplot ------------------------
# Ordem das amostras
levels(df$Amostra)
table(df$Amostra)

# Remover níveis com contagem zero 
?droplevels
df$Amostra = droplevels(df$Amostra)

levels(df$Amostra)

# Remover espaços no final
df$Amostra = as.factor(trimws(df$Amostra, which = c('right')))
levels(df$Amostra)

df$Amostra = ordered(df$Amostra, 
                          levels = c(	"PFM  0.50% Bentonita Nacional",	
                                      "PFNM 1 mm 0.15% TPP", 
                                      "PFNM 1 mm 0.04% TPP",                                           
                                      "PFNM 1 mm 0.15% Arkomon",  
                                      "PFNM 1 mm 0.04% Arkomon", 
                                      "PFNM 1 mm 0.12% PEO",                                           
                                      "PFNM 1 mm 0.04% PEO",
                                      "PFNM 1 mm 0.12% Amido", 
                                      "PFNM 1 mm 0.04% Amido", 
                                      "PFNM 1 mm 0.04% Peridur 0.04% NaOH",
                                      "PFNM 1 mm 0.12% Peridur 0.02% NaOH", 
                                      "PFNM 1 mm 0.04% Peridur 0.02% NaOH",                            
                                      "PFNM 1 mm 0.50% Bentonita Nacional",                            
                                      "PFNM 1 mm 0.50% Bentonita Nacional 0.04% TPP",                  
                                      "PFNM 1 mm 0.50% Bentonita Nacional 0.04% Arkomon",              
                                      "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur",
                                      "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur 0.04% Arkomon",           
                                      "PFNM 1 mm 1.00% Bentonita Nacional 0.06% Peridur 0.02% NaOH",
                                      "PFNM 1 mm 0.70% Bentonita Nacional 0.06% Peridur 0.02% NaOH",   
                                      "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur 0.02% NaOH", 
                                      "PFNM 1 mm 0.35% Bentonita Nacional 0.06% Peridur 0.02% NaOH"
                          ))
df$Amostra = fct_rev(df$Amostra)
levels(df$Amostra)

str(df)

# Para plot
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do t?tulo
        axis.line=element_line(), #Adicionar os eixos em todas facetas
        #panel.border = element_blank(), panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # axis.text.x=element_text(colour="black", size = 8),
        # axis.text.y=element_text(colour="black", size = 8), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

#Criar looping para plot 

# Não utilizar densidades e porosidades por enquanto
# variaveis = c(14,15,16,20,21,25,26,28)

# Utilizando tudo 
variaveis = c(14:16,20:26,28)
varnamesa = names(df[variaveis])
varnamesa

xnames = varnamesa[c(1:4,6:11)]
xnames
ynames = varnamesa[c(5,9:11)]
ynames
#Para legendas
# Com tudo 
axisnamesx = c('D10 (µm)','D50 (µm)','D90 (µm)',
              'Grau de Dispersão (%)',
              'Porosidades das pelotas queimadas (%)',
              'Porosidades das pelotas secas (%)',
              'Porosidades das pelotas verdes (%)',
              'Resistência à compressão queimada (kgf/pelota)',
              'Resistência à compressão seca (kgf/pelota)',
              'Resistência à compressão verde (kgf/pelota)')
titulosx = c('D10','D50','D90',
            'Grau de Dispersão',
            'Porosidades das \n pelotas queimadas',
            'Porosidades das \n pelotas secas',
            'Porosidades das \n pelotas verdes',
            'Resistência à \n compressão queimada',
            'Resistência à \n compressão seca',
            'Resistência à \n compressão verde')

axisnamesy = c('N° de Quedas (-)',
               'Resistência à compressão queimada (kgf/pelota)',
               'Resistência à compressão seca (kgf/pelota)',
               'Resistência à compressão verde (kgf/pelota)')
titulosy = c('N° de Quedas',
               'Resistência à \n compressão queimada',
             'Resistência à \n compressão seca',
             'Resistência à \n compressão verde')

legendax = data.frame(xnames,axisnamesx,titulosx)
legenday = data.frame(ynames,axisnamesy,titulosy)
legendax
legenday

# Mudar ordem dos fatores das famílias
levels(df$Familia2)
df$Familia2 = ordered(df$Familia2, 
                      levels = c('Bentonita Nacional','Peridur','Amido ou PEO','Arkomon ou TPP'))
levels(df$Familia2)

#Remover NA chato (por causa da amostra com peridur 0,04 % que não tem dados)
levels(df$Amostra)
df = df[!is.na(df$Resistencia.Seca),]
levels(df$Amostra)
df$Amostra = droplevels(df$Amostra)
levels(df$Amostra)
# Plots -------------------------------------------------------------------
# Cores 1
#Substituir os c?digos A e B pelos tamanhos das pelotas reais
df$AB = sub("A","-16+12,5 mm",df$AB)
df$AB = sub("B","-12,5+9,5 mm",df$AB)
df$AB = as.factor(df$AB)
df$AB = fct_rev(df$AB)
levels(df$AB)
cores = c('-16+12,5 mm' = '#3540f3',
          '-12,5+9,5 mm' = '#FF0000')

glist1 = list()
index = 0
# limites do looping
size = 6

# Níveis por minério
levels(df$Minerio)
# df$Minerio = as.integer(df$Minerio)
# levels(df$Minerio)

# Temos que amostras? a fictícia deve estar aqui
levels(df$Amostra)

# Como deixar alguns maiores?
# Todos em função da Dispersão
for (j in 1:length(xnames)){
  for (i in 1:length(ynames)) {
      index = index + 1
      glist1[[index]] = ggplot(df, aes_string(x = xnames[j], y = ynames[i])) + 
        geom_point(mapping = aes(fill = AB,  colour = AB, shape = Amostra, size = Minerio)) + 
        scale_shape_manual(values=c(1:18,22,23)) + #nlevels(df$Amostra 
        facet_rep_wrap(.~Familia1, repeat.tick.labels = TRUE) + 
        theme + 
        scale_color_manual(name = 'Granulometria das pelotas', values =  cores) + 
        scale_fill_manual(name = 'Granulometria das pelotas', values =  cores) + 
        xlab(legendax[j,2]) + 
        ylab(legenday[i,2]) + 
        ggtitle(paste(legenday[i,3],' X ',legendax[j,3], paste ='')) + 
        guides(shape = guide_legend(ncol = 1))
      print(varnamesa[j])
    }
}

glist1[[1]]
