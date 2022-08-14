setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
rm(list=ls())
df_A = read.csv('outputdata/df_plot_2.csv')
df_B = read.csv('outputdata/df_plot_B.csv')

# Se quiser utilizar a segunda granulometria:
df_A = df_B
# E mudar a pasta lá no final

# install.packages("lemon")
library(tidyverse)
library(lemon)
library(dplyr)
# Transformação de fatores e numeric
str(df_A)

df_A[names(df_A[c(1:13)])] = lapply(df_A[names(df_A[c(1:13)])], factor)

df_A[names(df_A[-c(1:13)])] = sapply(df_A[names(df_A[-c(1:13)])], as.numeric)

df_A$Amostra = as.factor(paste(df_A$Minerio,df_A$TS,
                                 df_A$Dose1,df_A$Aditivo1,
                                 df_A$Dose2,df_A$Aditivo2,
                                 df_A$Dose3,df_A$Aditivo3))
str(df_A)
# Mudar ordem de fatores --------------------------------------------------
#  Ordem das amostras para apresentação no boxplot ------------------------
# Ordem das amostras
levels(df_A$Amostra)
table(df_A$Amostra)

# Remover níveis com contagem zero 
?droplevels
df_A$Amostra = droplevels(df_A$Amostra)

levels(df_A$Amostra)

# Remover espaços no final
df_A$Amostra = as.factor(trimws(df_A$Amostra, which = c('right')))
levels(df_A$Amostra)

df_A$Amostra = ordered(df_A$Amostra, 
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
df_A$Amostra = fct_rev(df_A$Amostra)
levels(df_A$Amostra)

str(df_A)

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
varnamesa = names(df_A[variaveis])
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
            'Porosidades das pelotas queimadas',
            'Porosidades das pelotas secas',
            'Porosidades das pelotas verdes',
            'Resistência à compressão queimada',
            'Resistência à compressão seca',
            'Resistência à compressão verde')

axisnamesy = c('N° de Quedas (-)',
               'Resistência à compressão queimada (kgf/pelota)',
               'Resistência à compressão seca (kgf/pelota)',
               'Resistência à compressão verde (kgf/pelota)')
titulosy = c('N° de Quedas',
               'Resistência à compressão queimada',
             'Resistência à compressão seca',
             'Resistência à compressão verde')

legendax = data.frame(xnames,axisnamesx,titulosx)
legenday = data.frame(ynames,axisnamesy,titulosy)
legendax
legenday

#Remover NA chato (por causa da amostra com peridur 0,04 % que não tem dados)
levels(df_A$Amostra)
df_A = df_A[!is.na(df_A$Resistencia.Seca),]
levels(df_A$Amostra)
df_A$Amostra = droplevels(df_A$Amostra)
levels(df_A$Amostra)
# Plots -------------------------------------------------------------------
# Cores 1
df_A$Familia1 = as.factor(trimws(df_A$Familia1))
levels(df_A$Familia1)
df_A$Familia2 = as.factor(trimws(df_A$Familia2))
levels(df_A$Familia2)
cores = c('Amido ou PEO' = '#3540f3', 'PEO' = '#35e3f3',
          'Arkomon ou TPP' = '#f2f637','TPP' = '#f3a935', 
          'Bentonita Nacional' = '#35f33e',
          'Peridur' = '#f34f35')
glist1 = list()
index = 0
# limites do looping
init = 1
end = 4
size = 8
# Todos em função da Dispersão
for (j in init:length(xnames)){
  for (i in init:length(ynames)) {
      index = index + 1
      glist1[[index]] = ggplot(df_A, aes_string(x = xnames[j], y = ynames[i])) + 
        geom_point(mapping = aes(fill = Familia1,  colour = Familia1, shape = Amostra), size = size) + 
        scale_shape_manual(values=c(1:18,22)) + #nlevels(df_A$Amostra 
        facet_rep_wrap(.~Familia1, repeat.tick.labels = TRUE) + 
        theme + 
        scale_color_manual(name = 'Família', values =  cores) + 
        scale_fill_manual(name = 'Família', values =  cores) + 
        xlab(legendax[j,2]) + 
        ylab(legenday[i,2]) + 
        ggtitle(paste(legenday[i,3],' X ',legendax[j,3], paste ='')) + 
        guides(shape = guide_legend(ncol = 1))
      print(varnamesa[j])
    }
  }

print(glist1[[1]])

# Teste:
path = 'C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots'
path
index = 0; w = 12; h = 8; res = 80;
for (j in init:length(xnames)){
  for (i in init:length(ynames)){
      index = index + 1
      print(index)
      mypath = file.path(path,paste('Plots_Correlacoes/B/Familia1/',ynames[i],'_VS_',xnames[j]), sep = '')
      filename = paste(mypath, '.tiff', sep = '')
      tiff(filename, units = 'in', width = w, height = h, res = res)
      print(glist1[[index]])
      dev.off()
    }
  }

# Familia2 ----------------------------------------------------------------x  
glist2 = list()
index = 0
# Todos em função da Dispersão
for (j in init:length(xnames)){
  for (i in init:length(ynames)) {
      index = index + 1
      glist2[[index]] = ggplot(df_A, aes_string(x = xnames[j], y = ynames[i])) + 
        geom_point(mapping = aes(fill = Familia2,  colour = Familia2, shape = Amostra), size = size) + 
        scale_shape_manual(values=c(1:18,22)) + 
        facet_rep_wrap(.~Familia2, repeat.tick.labels = TRUE) + 
        theme + 
        scale_color_manual(name = 'Família', values =  cores) + 
        scale_fill_manual(name = 'Família', values =  cores) + 
        xlab(legendax[j,2]) + 
        ylab(legenday[i,2]) + 
        ggtitle(paste(legenday[i,3],' X ',legendax[j,3], paste ='')) + 
        guides(shape = guide_legend(ncol = 1))
  }
}
print(glist2[[1]])

path = 'C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots'
path
index = 0
multi = 1.2
index = 0; w = 12; h = 8; res = 80;
for (j in init:length(xnames)){
  for (i in init:length(ynames)){
      index = index + 1
      print(index)
      mypath = file.path(path,paste('Plots_Correlacoes/B/Familia2/',ynames[i],'_VS_',xnames[j]), sep = '')
      filename = paste(mypath, '.tiff', sep = '')
      tiff(filename, units = 'in', width = w*multi, height = h*multi, res = res*multi)
      print(glist2[[index]])
      dev.off()
  }
}

