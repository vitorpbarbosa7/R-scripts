rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
# setwd('C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomera??o de grossos/!Ana/Pelotizacao/Boxplot/R')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
#mydata = read_csv('Planilhas//database//database_no_dash.csv')
data = read.csv('indata/database.csv', sep = ",", header = FALSE, encoding = 'UTF-8')

#Transpor
data2 = transpose(data)
data2 = data2[-c(1),]
names(data2) = data[,1]
data2[c(28,29)] = NULL

#Transforma??o de colunas em linhas dos valores de interesse (R1 at? R2)
library(reshape2) ##Biblioteca que cont?m o reshape para transformar de linhas para colunas
mydata = melt(data = data2,
              id.vars = names(data2[c(1:17)]),
              measure.vars = names(data2[-c(1:17)])
)
names(mydata)[c(18:19)] = c("Replicata","Valor")
names(mydata)[8] = "AB"
names(mydata)[1] = 'Experimento'

# Converter todas colunas para fatores
mydata[names(mydata[c(1:18)])] = lapply(mydata[names(mydata[c(1:18)])], factor)

# Converter Valor para numérico
mydata$Valor = as.numeric(as.character(mydata$Valor))
summary(mydata)
str(mydata)

# Conforme informado pelo Chico, remover os ensaios R1, quando houver a duplicata, 
# portanto até o dia 11/11/2020 os ensaios a serem removidos são: 27,36,28
# Isto de acordo com a planilha de Controle

# Para aplicar o %in% sempre precisamos ter o dplyr carregado?
library(dplyr)
mydata = subset(mydata, !(Ensaio %in% c('E27','E36','E38')))
str(mydata$Ensaio)

# Agora precisamos obter datasets com as medias de cada experimento
# -- Resistencia Verde, Seca e Queimada
# -- Porosidade Verde, Seca e Queimada
# -- Dispersao
# -- D10, D50, D90

# Criar ID para cada amostra
mydata$ID = as.factor(paste(mydata$Minerio,
                            mydata$Dose1,mydata$Aditivo1,
                            mydata$Dose2,mydata$Aditivo2,
                            mydata$Dose3,mydata$Aditivo3,
                            mydata$AB))
# Verificar o número de níveis de IDS
str(mydata$ID)

# Substituirta
# Criar nova coluna porque esqueci no Excel: (fun??o paste permite isso )
mydata$Amostra = as.factor(paste(mydata$Minerio,mydata$TS,
                                 mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3))

mydata$Aditivo = as.factor(paste(mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3))

mydata$Aditivos = as.factor(paste(mydata$Aditivo1,"e",mydata$Aditivo2))

#Inverter a ordem dos n?veis deste fator para de acordo com o desejado no plot
mydata$Aditivo = fct_rev(mydata$Aditivo) 
mydata$Amostra = fct_rev(mydata$Amostra)
mydata$Aditivos = fct_rev(mydata$Aditivos)
mydata$AB = fct_rev(mydata$AB)

#Converter para fator e n?merico as devidas colunas
# mydata[,c(-17)] = lapply(mydata[-c(17)], factor)
mydata$Valor = as.character(mydata$Valor)
mydata$Valor = as.numeric(mydata$Valor)

mydata$Experimento = as.factor(mydata$Experimento)

# Verificar quais Experimentos estão disponíveis
levels(mydata$Experimento)

# Não vamos trabalhar com Choque Térmico, nem Cura UV
levels(mydata$SubExp)
medias = subset(mydata, !(SubExp %in% c('Seca e Curada UV, Verde e Curada UV','Curada UV',
                                        'Choque Térmico')))
mydata$SubSubExp = NULL


# Não vamos trabalhar com TS que não seja PFM ou 1 mm
mydata = subset(mydata, (TS %in% c('1 mm', 'PFM')))

subdata = (subset(mydata, 
                  (Experimento %in% c('Resistencia','N° de Quedas') & TS == "1 mm") |
                    (Experimento %in% c('Resistencia','N° de Quedas') & TS == "PFM" & Aditivo1 == "Bentonita Nacional")))


# Remover Choque Térmico --------------------------------------------------
subdata = subset(subdata, 
                 !(SubExp %in% c('Choque Térmico','Curada UV','Seca e Curada UV','Verde e Curada UV')))

table(subdata$SubExp)

# Verificar se selecionou todos os TS desejados
table(subdata$TS)
# Verificar se selecionou corretamente os Experimentos
table(subdata$Experimento)

# Spread ------------------------------------------------------------------
# TESTE SPREAD ## FUNÇÃO LINDA MARAVILHOSA, RESOLVEU TODOS MEUS PROBLEMAS!!!
# Foi então utilizado o melt antes, depois o group_by com summarize para média e desvio padrão e aagora finalmente separando
# todas variáveis novmaente com a funçao spread() do maravilhoso tidyr
# mydata = subset(mydata, AB == 'A')
# Criar a variável referência de chave (os experimentos)
subdata$Exp = paste(subdata$Experimento,subdata$SubExp)

# Então drop experimento
subdata$Experimento = NULL
subdata$SubExp = NULL

# Drop ppara não dar pau lá no dplyr
# subdata$Replica = NULL
# subdata$Replicata = NULL
# subdata$Ensaio = NULL
# subdata$AB = NULL
subdata$Unidade = NULL
# subdata$ID = NULL

# Retirar valores nulos de alguns
subdata = subdata[!is.na(subdata$Valor),]

# Pacote que contém o spread()
library(tidyr)
?spread()

# A chave utilizada, das variáveis, será a variável criada Exp a partir de Experimento e SubExp
# O valor são as médias, que foram calculadas com o dplyr através de group_by e summarize
spreaddata = subdata %>%
  spread(key = Exp, value = Valor)

subdata = spreaddata

str(subdata)

# Plot --------------------------------------------------------------------
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'none',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do t?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # axis.text.x=element_text(colour="black", angle = 75, hjust = 1, size = 8),
        axis.text.y=element_text(colour="black"), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

subdata$TS = fct_rev(subdata$TS)
subdata$Aditivo1 = as.factor(subdata$Aditivo1)

levels(subdata$Aditivo1)
levels(subdata$Dose1)
subdata$Amostra = fct_rev(subdata$Amostra)
vline = as.numeric(medias[medias$TS == 'PFM',][4])

# subdata = subset(subdata, AB == 'A')

#Substituir os c?digos A e B pelos tamanhos das pelotas reais
subdata$AB = sub("A","-16+12,5 mm",subdata$AB)
subdata$AB = sub("B","-12,5+9,5 mm",subdata$AB)
#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)

A = "-16+12,5 mm"
B = "-12,5+9,5 mm"

names(subdata)[c(18:21)] = c('Quedas','Queimada','Seca','Verde')
variaveis = names(subdata[c(18,19,20,21)])

# Para poder iterar sobre as granulometrias das pelotas
granulist = c("-16+12,5 mm","-12,5+9,5 mm")

g = ggplot(subset(subdata, AB == granulist[2]), aes_string(x = 'Amostra', y = variaveis[1])) + 
  geom_boxplot(aes(color = Amostra, fill = Amostra), alpha = 0.3, outlier.size = 0.8) + 
  coord_flip() + 
  #facet_grid(.~TS, scales = "free") + 
  # geom_text(data = medias, aes(label = media, y = media + 0.01),size =4) +
  # geom_hline(yintercept = vline, color = 'red', linetype = 'dashed', size = 1) + 
  xlab("") + 
  ylab("N° de Quedas") + 
  ggtitle('Número de quedas das pelotas de granulometria A') + 
  #scale_color_manual(values =  cores) + 
  #scale_fill_manual(values =  cores) + 
  theme  
g

# Plots -------------------------------------------------------------------
glist1 = list()
index = 0
# limites do looping (Temos apenas 4 variáveis, resistencias verde, seca, queimada e número de quedas)
init = 1
end = 4
# Todos em função da Dispersão
for (j in init:end){
  # Looping para variar a granulometria das pelotas
  for (i in 1:length(granulist)){
    index = index + 1
    glist1[[index]] = ggplot(subset(subdata, AB == granulist[i]), 
                             aes_string(x = 'Amostra', y = variaveis[j])) + 
      geom_boxplot(aes(color = Amostra, fill = Amostra), alpha = 0.3, outlier.size = 0.8) + 
      coord_flip() + 
      theme
      # scale_x_continuous(limits=c(0,max(df_A[,varnamesa[j]]))) + # Selecionar max de acordo com a coluna
      # scale_y_continuous(limits=c(0,max(df_A[,varnamesa[i]]))) + 
      # xlab(legenda[j,2]) + 
      # ylab(legenda[i,2]) + 
      # ggtitle(paste(legenda[i,3],' X ',legenda[j,3], paste =''))
    # print(varnamesa[j])
    }
}
glist1[3]
path = 'C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots/Boxplots'
path
index = 0; w = 12; h = 8; res = 80;
for (j in init:end){
  # Looping para salvar as diferentes granulometrias das pelotas
  for (i in 1:length(granulist)){
    index = index + 1
    print(index)
    mypath = file.path(path,paste(variaveis[j],granulist[i],sep = '_'))
    filename = paste(mypath, '.tiff', sep = '')
    tiff(filename, units = 'in', width = w, height = h, res = res)
    print(glist1[[index]])
    dev.off()
  }
}

# dim = read.csv('dimensions.csv')
# 
# tiff('imgs/facet/Workshop/Resistencia_Verde_A.tiff', width = dim$w, height = dim$h, res = dim$res,
#      units = 'in')
# g
# dev.off()