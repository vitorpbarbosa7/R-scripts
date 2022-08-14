rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')

library(stringr)
library(dplyr)
library(lattice)
library(tidyverse)
library(lemon)

originaldata = read.csv('outputdata/df_long.csv', sep = ',', stringsAsFactors = TRUE)

subdata_a = subset(originaldata, AB == 'A')
# Criar as tais famílias
subdata_a$Aditivos = as.factor(paste(subdata_a$Aditivo1, subdata_a$Aditivo2, subdata_a$Aditivo3))
levels(subdata_a$Aditivos)
subdata_a$Familia1 = as.character(subdata_a$Aditivos)

# Familia Bentonita e Dispersantes dispersantes

str_detect(subdata_a$Aditivos, 'Arkomon')

# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
subdata_a_new = 
  subdata_a %>%
  mutate(
    Familia1 = case_when(
      str_detect(Aditivos, "Arkomon") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "PEO") ~ "Amido ou PEO",
      str_detect(Aditivos, "Amido") ~ "Amido ou PEO",
      str_detect(Aditivos, "Peridur") ~ "Peridur",
      TRUE ~ Familia1
    )
  )

subdata_a_new$Familia1 = as.factor(subdata_a_new$Familia1)
levels(subdata_a_new$Familia1)

# Familia 2 
# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
subdata_a_new$Familia2 = as.character(subdata_a$Aditivos)

levels(subdata_a_new$Familia2)

subdata_a_new = 
  subdata_a_new %>%
  mutate(
    Familia2 = case_when(
      str_detect(Aditivos, "Bentonita Nacional") ~ "Bentonita Nacional",
      str_detect(Aditivos, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "PEO") ~ "Amido ou PEO",
      str_detect(Aditivos, "Amido") ~ "Amido ou PEO",
      str_detect(Aditivo1, "Peridur") ~ "Peridur",
      TRUE ~ Familia2
    )
  )
subdata_a_new$Familia2 = as.factor(subdata_a_new$Familia2)
levels(subdata_a_new$Familia2)

# Reordenando 
subdata_a = subdata_a_new
subdata_a = subdata_a[, c(1:17,19:23,18)]

# TESTE SPREAD ## FUNÇÃO LINDA MARAVILHOSA, RESOLVEU TODOS MEUS PROBLEMAS!!!
# Foi então utilizado o melt antes, depois o group_by com summarize para média e desvio padrão e aagora finalmente separando
# todas variáveis novmaente com a funçao spread() do maravilhoso tidyr

# Criar a variável referência de chave (os experimentos)
subdata_a$Exp = paste(subdata_a$Experimento,subdata_a$SubExp)

# Remover colunas para a key ficar correta
subdata_a$Replica = NULL
subdata_a$Ensaio = NULL
# subdata_a$Replicata = NULL
subdata_a$Unidade = NULL

# Então drop experimento
subdata_a$Experimento = NULL
subdata_a$SubExp = NULL
subdata_a$desvpad = NULL

# Pacote que contém o spread()
library(tidyr)
?spread()

# A chave utilizada, das variáveis, será a variável criada Exp a partir de Experimento e SubExp
# O valor são as médias, que foram calculadas com o dplyr através de group_by e summarize
spreaddata = subdata_a %>%
  spread(key = Exp, value = Valor)


# Lattice ----------------------------------------------------------------
names(spreaddata)

library(stringr)
names(spreaddata)[c(18:33)] = str_replace_all(names(spreaddata)[c(18:33)], '\\s','\\_')
names(spreaddata)[c(18:33)] = str_replace_all(names(spreaddata)[c(18:33)], '\\°','\\_')
names(spreaddata)

spreaddata[c(18:24,31,33)] = NULL

names(spreaddata)

# Looping para plot -------------------------------------------------------
# Utilizando tudo 
variaveis = c(18:length(names(spreaddata)))
varnamesa = names(spreaddata[variaveis])
varnamesa

ynames = varnamesa[c(1,5:7)]
xnames = varnamesa[c(1:4)]

# Retirar 

#Para legendas
# Com tudo 
axisnames = c('N° de Quedas (-)',
              'Porosidade queimada (%)','Porosidade seca (%)','Porosidade verde (%)',
              'Resistência queimada (kgf/pelota)','Resistência seca (kgf/pelota)',
              'Resistência verde (kgf/pelota)')


titulos = c('N° de quedas',
            'Porosidade queimada',
            'Porosidade seca','Porosidade verde',
            'Resistência queimada','Resistência seca','Resistência verde')

legenda = data.frame(varnamesa,axisnames,titulos)

# Plots -------------------------------------------------------------------
size = 5
# Para plot
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do t?tulo
        axis.line=element_line(), #Adicionar os eixos em todas facetas
        #panel.border = element_blank(), panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # axis.text.x=element_text(colour="black", angle = 75, hjust = 1, size = 8),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face
glist1 = list()
index = 0
# limites do looping
init = 1
end = length(varnamesa)
# Todos em função da Dispersão
for (j in init:end){
  for (i in init:end) {
    if (i != j){
      index = index + 1
      glist1[[index]] = ggplot(spreaddata, aes_string(x = varnamesa[j], y = varnamesa[i])) + 
        geom_point(mapping = aes(fill = Aditivo1,  colour = Aditivo1, shape = ID), size = size) + 
        # scale_shape_manual(values=1:nlevels(spreaddata$ID)) + 
        facet_rep_wrap(.~Familia1, repeat.tick.labels = TRUE) + 
        theme + 
        # scale_x_continuous(limits=c(0,max(spreaddata[,varnamesa[j]]))) + # Selecionar max de acordo com a coluna
        # scale_y_continuous(limits=c(0,max(spreaddata[,varnamesa[i]]))) + 
        xlab(legenda[j,2]) + 
        ylab(legenda[i,2]) + 
        ggtitle(paste(legenda[i,3],' X ',legenda[j,3], paste ='')) + 
        guides(shape = guide_legend(ncol = 1))
      print(varnamesa[j])
    }
  }
}

path = 'C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots'
path
index = 0; w = 12; h = 8; res = 80;
for (j in init:end){
  for (i in init:end){
    if (i != j){
      index = index + 1
      print(index)
      mypath = file.path(path,paste('Lattice/Familia1/',varnamesa[j],'_VS_',varnamesa[i]), sep = '')
      filename = paste(mypath, '.tiff', sep = '')
      tiff(filename, units = 'in', width = w, height = h, res = res)
      print(glist1[[index]])
      dev.off()
    }
  }
}

# Familia2 ----------------------------------------------------------------x  
glist2 = list()
index = 0
# Todos em função da Dispersão
for (j in init:end){
  for (i in init:end) {
    if (i != j){
      index = index + 1
      glist2[[index]] = ggplot(spreaddata, aes_string(x = varnamesa[j], y = varnamesa[i])) + 
        geom_point(mapping = aes(fill = Aditivo1,  colour = Aditivo1, shape = ID), size = size) + 
        # scale_shape_manual(values=1:nlevels(spreaddata$ID)) + 
        facet_rep_wrap(.~Familia2, repeat.tick.labels = TRUE) + 
        theme + 
        # scale_x_continuous(limits=c(0,max(spreaddata[,varnamesa[j]]))) + # Selecionar max de acordo com a coluna
        # scale_y_continuous(limits=c(0,max(spreaddata[,varnamesa[i]]))) + 
        xlab(legenda[j,2]) + 
        ylab(legenda[i,2]) + 
        ggtitle(paste(legenda[i,3],' X ',legenda[j,3], paste ='')) + 
        guides(shape = guide_legend(ncol = 1))
    }
  }
}

path = 'C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots'
path
index = 0
for (j in init:end){
  for (i in init:end){
    if (i != j){
      index = index + 1
      print(index)
      mypath = file.path(path,paste('Lattice/Familia2/',varnamesa[j],'_VS_',varnamesa[i]), sep = '')
      filename = paste(mypath, '.tiff', sep = '')
      tiff(filename, units = 'in', width = w, height = h, res = res)
      print(glist2[[index]])
      dev.off()
    }
  }
}


xyplot(Resistencia_Verde ~ Porosidade_Verde, data = spreaddata, 
       groups = Familia1,
       auto.key = TRUE, 
       pch = 16,
       scales = list(y=list(relation='free')))

xyplot(Resistencia_Seca ~ Porosidade_Seca, data = spreaddata, 
       groups = Familia1,
       auto.key = TRUE, 
       pch = 16,
       scales = list(y=list(relation='free')))

xyplot(Resistencia_Queimada ~ Porosidade_Queimada, data = spreaddata, 
       groups = Familia1,
       auto.key = TRUE, 
       pch = 16,
       scales = list(y=list(relation='free')))

# Por Família
xyplot(Resistencia_Seca ~ Porosidade_Seca | Familia1, data = spreaddata, 
       groups = Amostra,
       auto.key = TRUE, 
       pch = 16,
       scales = list(y=list(relation='free')))



