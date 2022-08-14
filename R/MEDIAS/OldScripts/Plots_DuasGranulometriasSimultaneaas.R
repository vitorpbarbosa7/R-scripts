setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
rm(list=ls())

# install.packages("lemon")
library(tidyverse)
library(lemon)
# library(dplyr)

df = read.csv('outputdata/df_plot_AB_Referencias.csv')

str(df)


# Mudar nome das colunas
names(df) = str_replace_all(names(df), '\\.','_')


# Converter para fator
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

# Deixar Bentonita Nacional para depois nos nomes
vetorpattern = c("PFNM 1 mm 0.50% Bentonita Nacional 0.04% TPP","PFNM 1 mm 0.50% Bentonita Nacional 0.04% Arkomon",
                 "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur 0.04% Arkomon","PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur",
                 "PFNM 1 mm 1.00% Bentonita Nacional 0.06% Peridur 0.02% NaOH","PFNM 1 mm 0.70% Bentonita Nacional 0.06% Peridur 0.02% NaOH",
                 "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur 0.02% NaOH","PFNM 1 mm 0.35% Bentonita Nacional 0.06% Peridur 0.02% NaOH")

vetorreplace = c("PFNM 1 mm 0.04% TPP 0.50% Bentonita Nacional","PFNM 1 mm 0.04% Arkomon 0.50% Bentonita Nacional",
                 "PFNM 1 mm  0.06% Peridur 0.50% Bentonita Nacional 0.04% Arkomon","PFNM 1 mm 0.06% Peridur 0.50% Bentonita Nacional",
                 "PFNM 1 mm 0.06% Peridur 1.00% Bentonita Nacional 0.02% NaOH","PFNM 1 mm 0.06% Peridur 0.70% Bentonita Nacional 0.02% NaOH",
                 "PFNM 1 mm 0.06% Peridur 0.50% Bentonita Nacional 0.02% NaOH","PFNM 1 mm 0.06% Peridur 0.35% Bentonita Nacional 0.02% NaOH")

for (k in 1:length(vetorpattern)){
  df$Amostra = as.factor(gsub(vetorpattern[k],vetorreplace[k], df$Amostra))
}
levels(df$Amostra)
# Deixar na ordem desejada  
df$Amostra = ordered(df$Amostra, 
                     levels = c("PFM  0.50% Bentonita Nacional",
                                 "PFNM 1 mm 0.50% Bentonita Nacional",
                                 "PFNM 1 mm 0.04% TPP 0.50% Bentonita Nacional",	
                                 "PFNM 1 mm 0.15% TPP", 
                                 "PFNM 1 mm 0.04% TPP",
                                 "PFNM 1 mm 0.04% Arkomon 0.50% Bentonita Nacional",                                           
                                 "PFNM 1 mm 0.15% Arkomon",  
                                 "PFNM 1 mm 0.04% Arkomon", 
                                 "PFNM 1 mm 0.12% PEO",                                           
                                 "PFNM 1 mm 0.04% PEO",
                                 "PFNM 1 mm 0.12% Amido", 
                                 "PFNM 1 mm 0.04% Amido", 
                                 "PFNM 1 mm 0.12% Peridur 0.04% NaOH", 
                                 "PFNM 1 mm 0.12% Peridur 0.02% NaOH", 
                                 "PFNM 1 mm 0.04% Peridur 0.02% NaOH",
                                "PFNM 1 mm  0.06% Peridur 0.50% Bentonita Nacional 0.04% Arkomon",
                                "PFNM 1 mm 0.06% Peridur 0.50% Bentonita Nacional",
                                "PFNM 1 mm 0.06% Peridur 1.00% Bentonita Nacional 0.02% NaOH",
                                "PFNM 1 mm 0.06% Peridur 0.70% Bentonita Nacional 0.02% NaOH",
                                "PFNM 1 mm 0.06% Peridur 0.50% Bentonita Nacional 0.02% NaOH",
                                "PFNM 1 mm 0.06% Peridur 0.35% Bentonita Nacional 0.02% NaOH"))
df$Amostra = fct_rev(df$Amostra)
levels(df$Amostra)

str(df)

# Retirar a amostra desgraçada, merda
df = df[df$Amostra != 'PFNM 1 mm 0.12% Peridur 0.04% NaOH',]

levels(df$Amostra)

df$Amostra = droplevels(df$Amostra)
levels(df$Amostra)

# Tem merda de fator na variavel facet? 
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
names(df)
variaveis = c(14:16,20:26,28)
varnamesa = names(df[variaveis])
varnamesa

xnames = varnamesa[c(1:7,9:11)]
xnames
ynames = varnamesa[c(8:11)]
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
levels(df$Familia)
df$Familia = ordered(df$Familia, 
                      levels = c('Peridur','Amido ou PEO','Arkomon ou TPP'))
levels(df$Familia)

#Remover NA chato (por causa da amostra com peridur 0,04 % que não tem dados)
levels(df$Amostra)

# df = subset(df, !(Amostra %in% c('PFNM 1 mm 0.04% Peridur 0.02% NaOH')))
# 
# levels(df$Amostra)
# 
# df$Amostra = droplevels(df$Amostra)
# 
# df$Amostra = as.factor(df$Amostra) 
# levels(df$Amostra)

# Plots -------------------------------------------------------------------
# Cores 1
#Substituir os c?digos A e B pelos tamanhos das pelotas reais
df$AB = sub("A","-16+12,5 mm",df$AB)
df$AB = sub("B","-12,5+9,5 mm",df$AB)
df$AB = as.factor(df$AB)
df$AB = fct_rev(df$AB)
levels(df$AB)
coresgranu = c('-16+12,5 mm' = '#3540f3',
          '-12,5+9,5 mm' = '#FF0000')
coresref = c('Não' = '#3540f3',
             'Sim' = '#FF0000')


# Utilizar a Referência para diferenciar tamanhos
levels(df$Referencia)
# df$Referencia = fct_rev(df$Referencia)

glist1 = list()
index = 0
# limites do looping
init = 1
end = 4
size = 6
ynames
xnames

# Tem merdaa de fator ?
str(df)

# Todos em função da Dispersão
for (j in init:length(xnames)){
  for (i in init:length(ynames)) {
    index = index + 1
    glist1[[index]] = ggplot(df, aes_string(x = xnames[j], y = ynames[i])) + 
      geom_point(mapping = aes(colour = Referencia, shape = Amostra, alpha = AB), size = 3, stroke = 1.3) + 
      scale_size_discrete(range = c(5,10)) +
      # guides(alpha = guide_legend(override.aes = list(alpha=0.7))) +
      scale_alpha_manual(name = 'Granulometria das pelotas', values = c(1,0.5)) + 
      scale_shape_manual(values=c(1:14,35:38,17,19)) + #nlevels(df$Amostra 
      facet_rep_wrap(.~Familia, repeat.tick.labels = TRUE, nrow = 2, ncol = 2) + 
      theme + 
      scale_color_manual(name = 'Referência', values =  coresref) +
      # scale_fill_manual(name = 'Granulometria das pelotas', values =  cores) + 
      xlab(legendax[j,2]) + 
      ylab(legenday[i,2]) + 
      ggtitle(paste(legenday[i,3],' X ',legendax[j,3], paste ='')) + 
      guides(shape = guide_legend(ncol = 1, order = 3),
             color = guide_legend(order = 1),
             alpha = guide_legend(order = 2))
    print(varnamesa[j])
  }
}

print(glist1[[8]])


path = 'C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots'
path
endj = length(xnames)
endi = length(ynames)

# endj = 2
# endi = 2
index = 0; w = 14; h = 10; res = 120;
for (j in init:endj){
  for (i in init:endi){
      index = index + 1
      print(index)
      mypath = file.path(path,paste('Plots_Correlacoes/Referencias/',ynames[i],'_VS_',xnames[j]), sep = '')
      filename = paste(mypath, '.tiff', sep = '')
      tiff(filename, units = 'in', width = w, height = h, res = res)
      print(glist1[[index]])
      dev.off()
    }
  }

