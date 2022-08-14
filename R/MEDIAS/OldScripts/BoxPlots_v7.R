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
mydata = subset(mydata, !(Ensaio %in% c('E27','E18','E13','E26','E36','E38','E28')))
mydata$Ensaio = as.factor(droplevels(mydata$Ensaio))
levels(mydata$Ensaio)
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

mydata$Amostra = as.factor(paste(mydata$Minerio,mydata$TS,
                                 mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3))
levels(mydata$Amostra)

mydata$Aditivo = as.factor(paste(mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3))

mydata$Aditivos = as.factor(paste(mydata$Aditivo1,"e",mydata$Aditivo2))

#Inverter a ordem dos n?veis deste fator para de acordo com o desejado no plot
mydata$Aditivo = fct_rev(mydata$Aditivo) 
mydata$Amostra = fct_rev(mydata$Amostra)
levels(mydata$Amostra)
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
mydata = subset(mydata, !(SubExp %in% c('Seca e Curada UV, Verde e Curada UV','Curada UV',
                                        'Choque Térmico')))
mydata$SubSubExp = NULL

# Não vamos trabalhar com TS que não seja PFM ou 1 mm
mydata = subset(mydata, (TS %in% c('1 mm', '')))

# Criar as famílias -------------------------------------------------------
mydata$Aditivos = as.factor(paste(mydata$Aditivo1, mydata$Aditivo2, mydata$Aditivo3))
levels(mydata$Aditivos)

mydata$Familia = as.character(mydata$Aditivos)
levels(mydata$Aditivos)

# Familia Bentonita e Dispersantes dispersantes

library(stringr)

levels(mydata$Aditivos)

# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
mydata_new = 
  mydata %>%
  mutate(
    Familia = case_when(
      str_detect(Aditivos, "Arkomon") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "PEO") ~ "Amido ou PEO",
      str_detect(Aditivos, "Amido") ~ "Amido ou PEO",
      str_detect(Aditivos, "Peridur") ~ "Peridur",
      str_detect(Aditivos, "Bentonita Nacional") ~ "Referências",
      TRUE ~ Familia
    )
  )
mydata_new$Familia = as.factor(mydata_new$Familia)
levels(mydata_new$Familia)

# Concertar o Peridur que tem Arkomon
mydata_new$Familia = as.character(mydata_new$Familia)
mydata_new = 
  mydata_new %>%
  mutate(
    Familia = case_when(
      str_detect(Aditivos, 'Peridur') ~ 'Peridur',
      TRUE ~ Familia
    )
  )

# Após o processo, voltar para o nome de objeto mydata
mydata = mydata_new
# Para poder trabalhar com porosidade e outras variávieis em outros scripts, não tirar ainda
subdata2 = (subset(mydata, 
                  (TS == "1 mm") |
                    (Minerio == "PFM" & Aditivo1 == "Bentonita Nacional")))

write.csv(subdata2, file = 'outputdata/subdata_long.csv', row.names = FALSE)

# Continuar a analise desses boxplots
subdata = (subset(mydata, 
                  (Experimento %in% c('Resistencia','Quedas') & TS == "1 mm") |
                    (Experimento %in% c('Resistencia','Quedas') & Minerio == "PFM" & Aditivo1 == "Bentonita Nacional")))

table(subdata$Experimento)

# Verificar se selecionou todos os TS desejados
table(subdata$TS)
# Verificar se selecionou corretamente os Experimentos
table(subdata$Experimento)

# Plot --------------------------------------------------------------------
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 20), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do t?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour="black", size = 22),
        axis.text.y=element_text(colour="black", size = 18), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

subdata$TS = fct_rev(subdata$TS)
subdata$Aditivo1 = as.factor(subdata$Aditivo1)

levels(subdata$Aditivo1)
levels(subdata$Dose1)
subdata$Amostra = fct_rev(subdata$Amostra)

#  Ordem das amostras para apresentação no boxplot ------------------------
# Ordem das amostras
levels(subdata$Amostra)
table(subdata$Amostra)

# Remover níveis com contagem zero 
?droplevels
subdata$Amostra = droplevels(subdata$Amostra)

levels(subdata$Amostra)

# Remover espaços no final
subdata$Amostra = as.factor(trimws(subdata$Amostra, which = c('right')))
levels(subdata$Amostra)


subdesgraca = subdata

# Deixar Bentonita Nacional para depois nos nomes
vetorpattern = c("PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur 0.02% NaOH",
                 "PFNM 1 mm 0.50% Bentonita Nacional 0.04% TPP",
                 "PFNM 1 mm 0.50% Bentonita Nacional 0.04% Arkomon",
                 "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur 0.04% Arkomon",
                 "PFNM 1 mm 0.50% Bentonita Nacional 0.06% Peridur",
                 "PFNM 1 mm 1.00% Bentonita Nacional 0.06% Peridur 0.02% NaOH",
                 "PFNM 1 mm 0.70% Bentonita Nacional 0.06% Peridur 0.02% NaOH",
                 "PFNM 1 mm 0.35% Bentonita Nacional 0.06% Peridur 0.02% NaOH")

vetorreplace = c("PFNM 1 mm 0.04% TPP 0.50% Bentonita Nacional",
                 "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.50% Bentonita Nacional",
                 "PFNM 1 mm 0.04% Arkomon 0.50% Bentonita Nacional",
                 "PFNM 1 mm 0.06% Peridur 0.04% Arkomon 0.50% Bentonita Nacional",
                 "PFNM 1 mm 0.06% Peridur 0.50% Bentonita Nacional",
                 "PFNM 1 mm 0.06% Peridur 0.02% NaOH 1.00% Bentonita Nacional",
                 "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.70% Bentonita Nacional",
                 "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.35% Bentonita Nacional")

levels(subdata$Amostra)

for (k in 1:length(vetorpattern)){
  subdata$Amostra = as.factor(str_replace(subdata$Amostra,
                                          vetorpattern[k],
                                          vetorreplace[k]))
  print(vetorpattern[k])
  print(vetorreplace[k])
}

levels(subdata$Amostra)

# Deixar na ordem desejada  
subdata$Amostra = ordered(subdata$Amostra, 
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
                                     "PFNM 1 mm 0.12% Peridur 0.02% NaOH", 
                                     "PFNM 1 mm 0.04% Peridur 0.02% NaOH",
                                     "PFNM 1 mm 0.06% Peridur 0.04% Arkomon 0.50% Bentonita Nacional",
                                     "PFNM 1 mm 0.06% Peridur 0.50% Bentonita Nacional",
                                     "PFNM 1 mm 0.06% Peridur 0.02% NaOH 1.00% Bentonita Nacional",
                                     "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.70% Bentonita Nacional",
                                     "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.50% Bentonita Nacional",
                                     "PFNM 1 mm 0.06% Peridur 0.02% NaOH 0.35% Bentonita Nacional"))
subdata$Amostra = fct_rev(subdata$Amostra)
levels(subdata$Amostra)

str(subdata)

levels(subdata$Amostra)

# Reverter a ordem:
df$Amostra = fct_rev(df$Amostra)

#Substituir os c?digos A e B pelos tamanhos das pelotas reais
subdata$AB = sub("A","-16 mm +12,5 mm",subdata$AB)
subdata$AB = sub("B","-12,5 mm +9,5 mm",subdata$AB)
#Remove the NAs returned (valores nulos que n?o existem)
#mydata$Amostras = sub("NA,"",mydata$Amostras)

# Para facilitar a iteração nos experimentos com apenas uma coluna
subdata$Exp = as.factor(paste(subdata$Experimento,subdata$SubExp))
levels(subdata$Exp)
str(subdata)
levels(subdata$Exp)

# Para poder iterar sobre os diferentes experimentos
experimentos = c('Quedas ','Resistencia Verde','Resistencia Seca','Resistencia Queimada')

# Para poder iterar sobre as granulometrias das pelotas
granulist = c("-16 mm +12,5 mm","-12,5 mm +9,5 mm")

# Criar o objeto legenda para poder ter título e nome dos eixos corretamente
axisnames = c('Número de Quedas (-)',
              'Resistência à compressão verde (kgf/pelota)',
              'Resistência à compressão seca (kgf/pelota)',
              'Resistência à compressão queimada (kgf/pelota)')
titulos = c('Número de Quedas',
            'Resistência à compressão verde',
            'Resistência à compressão seca',
            'Resistência à compressão queimada')
legenda = data.frame(cbind(experimentos,axisnames,titulos))
legenda
# Plots -------------------------------------------------------------------
glist1 = list(); subdatalist = list(); mediaslist = list();
index = 0

# Cores 
subdata$Familia = droplevels(subdata$Familia)
subdata$Familia = as.factor(subdata$Familia)
levels(subdata$Familia)
subdata$Familia = ordered(subdata$Familia, 
                          levels = c('Peridur','Amido ou PEO','Arkomon ou TPP','Referências'))
levels(subdata$Familia)
str(subdata)
cores = c('Amido ou PEO' = '#3540f3', 'Arkomon ou TPP' = '#f2f637', 'Referências' = '#f34f35',
          'Peridur' = '#35f33e')

# limites do looping (Temos apenas 4 variáveis, resistencias verde, seca, queimada e número de quedas)
init = 1
end = 4
loopingdata = subdata
levels(loopingdata$Amostra)

levels(loopingdata$Familia)

# Looping para variar a granulometria das pelotas
for (i in 1:length(granulist)){
  # Looping para variar os subsets
  for (j in init:end){
  
    index = index + 1 # Gravar os objetos do looping em listas
    # subset --------------------------------------------------------------------
    subdata = (subset(loopingdata, 
                      (Exp == experimentos[j] & AB == granulist[i] & TS == "1 mm") 
                      |
                      Exp == experimentos[j] & AB == granulist[i] & Minerio == "PFM" & Aditivo1 == "Bentonita Nacional"))
    
    subdatalist[[index]] = subdata
    
    #Para poder agrupar por SubSubExp que cont?m a especifica??o de temperaturas do choque t?rmico
    medias = subdata %>%
      group_by(Ensaio, TS, Amostra) %>%
      summarise_at(vars(Valor), list(name = mean))
    nft = length(medias)
    names(medias)[nft] = 'media'
    medias[nft] = round(medias[nft], 2)
    vline = as.numeric(medias[medias$TS == '',][4])
    
    mediaslist[[index]] = medias
    
    glist1[[index]] = ggplot(subdata, aes(x = reorder(Amostra, desc(Amostra)), 
                                                      y = Valor)) + 
      geom_boxplot(aes(color = Familia, fill = Familia), alpha = 0.5, outlier.size = 0.8) + 
      coord_flip() + 
      geom_text(data = medias, aes(label = media, y = media + 0.01),size =6) +
      geom_hline(yintercept = vline, color = 'red', linetype = 'dashed', size = 1) + 
      xlab("") + 
      ylab(legenda[j,2]) + #axisnames do dataframe legenda 
      ggtitle(paste(legenda[j,3],'das pelotas de granulometria',granulist[i])) + 
      scale_color_manual(name = 'Família', values =  cores) + 
      scale_fill_manual(name = 'Família', values =  cores) + 
      # scale_fill_discrete(name = 'Família') + 
      theme  
    }
}
# Teste para ver se funcionou
glist1[[2]]


path = 'C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots/Boxplots/Res'
path
index = 0; w = 18; h = 16; res = 200;
variaveis = c('Quedas','Res_Verde','Res_Seca','Res_Queimada')
# Looping para salvar as diferentes granulometrias das pelotas
for (i in 1:length(granulist)){
  for (j in init:end){
    print(index)
    index = index + 1
    print(index)
    mypath = file.path(path,paste(variaveis[j],granulist[i],sep = '_'))
    filename = paste(mypath, '.tiff', sep = '')
    tiff(filename, units = 'in', width = w, height = h, res = res)
    print(glist1[[index]])
    dev.off()
  }
}
