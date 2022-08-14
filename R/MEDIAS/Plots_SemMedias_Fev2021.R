setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
rm(list=ls())

# install.packages("lemon")
library(tidyverse)
library(lemon)
library(dplyr)

df = read.csv('outputdata/df_plot_AB_Referencias_Fev2021.csv')

str(df)

# Mudar nome das colunas
names(df) = str_replace_all(names(df), '\\.','_')

# Converter para fator
df[names(df[c(1:20)])] = lapply(df[names(df[c(1:20)])], factor)

df[names(df[-c(1:20)])] = sapply(df[names(df[-c(1:20)])], as.numeric)

df$Amostra = as.factor(paste(df$Minerio,df$TS,
                                 df$Dose1,df$Aditivo1,
                                 df$Dose2,df$Aditivo2,
                                 df$Dose3,df$Aditivo3))
str(df)

# Mudar ordem de fatores --------------------------------------------------
#  Ordem das amostras para apresentaÃ§Ã£o no boxplot ------------------------
# Ordem das amostras
levels(df$Amostra)
table(df$Amostra)

# Remover nÃ­veis com contagem zero 
?droplevels
df$Amostra = droplevels(df$Amostra)

levels(df$Amostra)

# Remover espaÃ§os no final
df$Amostra = as.factor(trimws(df$Amostra, which = c('right')))
levels(df$Amostra)

# # Deixar Bentonita Nacional para depois nos nomes
# vetorpattern = c("PFNM 1 mm 0.50% Bentonita Nacional 0.04% TPP",
#                  "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur 0.02% NaOH",
#                  "PFNM 1 mm 0.50% Bentonita Nacional 0.04% Arkomon",
#                  "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur 0.04% Arkomon",
#                  "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur",
#                  "PFNM 1 mm 1.00% Bentonita Nacional 0.06% Peridur 0.02% NaOH",
#                  "PFNM 1 mm 0.70% Bentonita Nacional 0.06% Peridur 0.02% NaOH",
#                  "PFNM 1 mm 0.35% Bentonita Nacional 0.06% Peridur 0.02% NaOH")
# 
# vetorreplace = c("PFNM 1 mm 0.04% TPP 0.50% Bentonita Nacional",
#                  "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.50% Bentonita Nacional",
#                  "PFNM 1 mm 0.04% Arkomon 0.50% Bentonita Nacional",
#                  "PFNM 1 mm 0.06% Peridur 0.04% Arkomon 0.50% Bentonita Nacional",
#                  "PFNM 1 mm 0.06% Peridur 0.50% Bentonita Nacional",
#                  "PFNM 1 mm 0.06% Peridur 0.02% NaOH 1.00% Bentonita Nacional",
#                  "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.70% Bentonita Nacional",
#                  "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.35% Bentonita Nacional")
# 
# levels(df$Amostra)
# 
# for (k in 1:length(vetorpattern)){
#   df$Amostra = as.factor(str_replace(df$Amostra,
#                                           vetorpattern[k],
#                                           vetorreplace[k]))
#   print(vetorpattern[k])
#   print(vetorreplace[k])
# }
# 
# levels(df$Amostra)
# 
# # Deixar na ordem desejada  
# df$Amostra = ordered(df$Amostra, 
#                           levels = c("PFM  0.50% Bentonita Nacional",
#                                      "PFNM 1 mm 0.50% Bentonita Nacional",
#                                      "PFNM 1 mm 0.04% TPP 0.50% Bentonita Nacional",	
#                                      "PFNM 1 mm 0.15% TPP", 
#                                      "PFNM 1 mm 0.04% TPP",
#                                      "PFNM 1 mm 0.04% Arkomon 0.50% Bentonita Nacional",                                           
#                                      "PFNM 1 mm 0.15% Arkomon",  
#                                      "PFNM 1 mm 0.04% Arkomon", 
#                                      "PFNM 1 mm 0.12% PEO",                                           
#                                      "PFNM 1 mm 0.04% PEO",
#                                      "PFNM 1 mm 0.12% Amido", 
#                                      "PFNM 1 mm 0.04% Amido", 
#                                      "PFNM 1 mm 0.12% Peridur 0.04% NaOH",
#                                      "PFNM 1 mm 0.12% Peridur 0.02% NaOH", 
#                                      "PFNM 1 mm 0.04% Peridur 0.02% NaOH",
#                                      "PFNM 1 mm 0.06% Peridur 0.04% Arkomon 0.50% Bentonita Nacional",
#                                      "PFNM 1 mm 0.06% Peridur 0.50% Bentonita Nacional",
#                                      "PFNM 1 mm 0.06% Peridur 0.02% NaOH 1.00% Bentonita Nacional",
#                                      "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.70% Bentonita Nacional",
#                                      "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.50% Bentonita Nacional",
#                                      "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.35% Bentonita Nacional"))
# df$Amostra = fct_rev(df$Amostra)
# levels(df$Amostra)
# 
# str(df)
# 
# levels(df$Amostra)

# Retirar a amostra desgraÃ§ada, merda
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
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

#Criar looping para plot 

# NÃ£o utilizar densidades e porosidades por enquanto
# variaveis = c(14,15,16,20,21,25,26,28)

# Utilizando tudo 
names(df)
variaveis = c(21:23,27:33)
varnamesa = names(df[variaveis])
varnamesa

xnames = varnamesa[c(1:7,9:11)]
xnames
ynames = varnamesa[c(8:11)]
ynames
#Para legendas
# Com tudo 
axisnamesx = c('D10 (µm)','D50 (µm)','D90 (µm)',
              'Grau de Dispersãoo (%)',
              'Porosidades das pelotas queimadas (%)',
              'Porosidades das pelotas secas (%)',
              'Porosidades das pelotas verdes (%)',
              'Resistência à compressão queimada (kgf/pelota)',
              'Resistência à compressão seca (kgf/pelota)',
              'Resistência à compressão verde (kgf/pelota)')
titulosx = c('D10','D50','D90',
            'Grau de Dispersãoo',
            'Porosidades das \n pelotas queimadas',
            'Porosidades das \n pelotas secas',
            'Porosidades das \n pelotas verdes',
            'Resistência à \n compressão queimada',
            'Resistência à \n compressão seca',
            'Resistência à \n compressão verde')

axisnamesy = c('N° de Quedas (-)',
               'Resistência à compressão queimada (kgf/pelota)',
               'Resistência à compressão seca (kgf/pelota)',
               'Resistência â compressão verde (kgf/pelota)')
titulosy = c('NÂ° de Quedas',
               'Resistência à \n compressãoo queimada',
             'Resistência à \n compressãoo seca',
             'Resistência à \n compressãoo verde')

legendax = data.frame(xnames,axisnamesx,titulosx)
legenday = data.frame(ynames,axisnamesy,titulosy)
legendax
legenday

# Mudar ordem dos fatores das famÃ­lias
levels(df$Familia)
df$Familia = ordered(df$Familia, 
                      levels = c('Peridur','Amido ou PEO','Arkomon ou TPP'))
levels(df$Familia)


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

# Substituir os nomes presentes em Referencia "sim" e "nao"

reflegend = c('Aglomerantes e/ou Aditivos em estudo','Amostras de referência com 0,5 % Bentonita Nacional')
df$Referencia = str_replace_all(df$Referencia, 'Não', reflegend[1])
df$Referencia = str_replace_all(df$Referencia, 'Sim', reflegend[2])
df$Referencia = as.factor(df$Referencia)

# Legenda puxando pelo vetor nÃ£o funcionou
coresref = c('Aglomerantes e/ou Aditivos em estudo' = '#3540f3',
              'Amostras de referência com 0,5 % Bentonita Nacional' = '#FF0000',
             '  ' = '#FFFF00')


# Utilizar a referência para diferenciar tamanhos
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

# ApÃ³s realizado todo esse prÃ©-processamento, salvar este dataset para poder utilizÃ¡-lo em outro Script
# sem perder a formataÃ§Ã£o e codificaÃ§Ã£o dos tipos de dados
save(df, file = 'datasets/df_Fev2021.Rdata')

# Verde x Seca x Quedas

# Enquanto não tem o nome certinho

library(ggrepel)
ggplot(df, aes(x = Resistencia_Seca, y = Resistencia_Verde, size = Quedas_)) + 
  geom_point(aes(shape = Amostra, alpha = AB, color = Referencia)) + 
  scale_size(range = c(2,8)) + 
  scale_shape_manual(values = c(1:14,35:38,39,40,17,19)) +
  scale_alpha_manual(name = 'Granulometria das pelotas', values = c(1,0.5)) +
  scale_color_manual(name = '', values =  coresref) +
  geom_text_repel(aes(label = Quedas_, size = Quedas_), force = 5) + 
  facet_rep_wrap(.~Familia, repeat.tick.labels = TRUE, nrow = 2, ncol = 2) + 
  theme_bw()
  

# Todos em funÃ§Ã£o da DispersÃ£o
for (j in init:length(xnames)){
  for (i in init:length(ynames)) {
    index = index + 1
    glist1[[index]] = ggplot(df, aes_string(x = xnames[j], y = ynames[i])) + 
      geom_point(mapping = aes(colour = Referencia, shape = Amostra, alpha = AB), size = 3, stroke = 1.3) + 
      scale_size_discrete(range = c(5,10)) +
      # guides(alpha = guide_legend(override.aes = list(alpha=0.7))) +
      scale_alpha_manual(name = 'Granulometria das pelotas', values = c(1,0.5)) + 
      scale_shape_manual(name = '', values=c(1:14,35:38,17,19)) + #nlevels(df$Amostra 
      facet_rep_wrap(.~Familia, repeat.tick.labels = TRUE, nrow = 2, ncol = 2) + 
      theme + 
      scale_color_manual(name = '', values =  coresref) +
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
index = 0; w = 14; h = 9; res = 140;
for (j in init:endj){
  for (i in init:endi){
      index = index + 1
      print(index)
      mypath = paste('outputdata/plots/Plots_Correlacoes/Referencias/',ynames[i],'_VS_',xnames[j], sep = '_')
      filename = paste(mypath, '.tiff', sep = '')
      tiff(filename, units = 'in', width = w, height = h, res = res)
      print(glist1[[index]])
      dev.off()
    }
}


