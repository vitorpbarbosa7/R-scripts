rm(list=ls())
# setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
setwd('C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomeração de grossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')

# Bibliotecas -------------------------------------------------------------
library(tidyverse) #Biblioteca que cont?m o poderoso ggplot e read_csv e n?o o raw built-in do R read.csv
library(data.table) #Para transpor os dados

# Importacao dos dados ----------------------------------------------------
data = read.csv('indata/database_3_2021_Abrev.csv', sep = ";", header = FALSE, encoding = 'UTF-8')

#Transpor
data2 = transpose(data)
data2 = data2[-c(1),]
names(data2) = data[,1]

#Transforma??o de colunas em linhas dos valores de interesse (R1 at? R2)
library(reshape2) ##Biblioteca que cont?m o reshape para transformar de linhas para colunas
mydata = melt(data = data2,
              id.vars = names(data2[c(1:22)]),
              measure.vars = names(data2[-c(1:22)])
)

names(mydata)[c(23:24)] = c("Replicata","Valor")
names(mydata)[8] = "AB"
names(mydata)[1] = 'Experimento'

# Converter todas colunas para fatores
mydata[names(mydata[c(1:23)])] = lapply(mydata[names(mydata[c(1:23)])], factor)

# Converter Valor para numà©rico
mydata$Valor = as.numeric(as.character(mydata$Valor))
summary(mydata)
str(mydata)

# Conforme informado pelo Chico, remover os ensaios R1, quando houver a duplicata, 
# portanto atà© o dia 11/11/2020 os ensaios a serem removidos sà£o: 27,36,28
# Isto de acordo com a planilha de Controle

# Para aplicar o %in% sempre precisamos ter o dplyr carregado?
library(dplyr)
# Retirar o Ensaio 06, pq era a refer?ncia, mas agora passou a ser o ensaio 46
# Decis?o informada no dia 04/02/2021

# Outros ensaios que ? n?o para considerar ? o 49 e o 52. 
# Endere?o do audio: C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomeraçào de grossos/!Ana/Pelotizacao/Boxplot/R/Dados_Originais/Resistencias/AUDIO
# No audio ele falou errado, que era 49 e 52, mas ? para desconsiderar 49 e 51. 
mydata = mydata[mydata$Ensaio %in% c('E30','E31','E32','E54','E57'),]

mydata = mydata[!mydata$SubExp %in% c('Queimada'),]

mydata$Ensaio = as.factor(droplevels(mydata$Ensaio))
levels(mydata$Ensaio)
str(mydata$Ensaio)

# Agora precisamos obter datasets com as medias de cada experimento
# -- Resistencia Verde, Seca e Queimada
# -- Porosidade Verde, Seca e Queimada
# -- Quedas

# Criar ID para cada amostra
mydata$ID = as.factor(paste(mydata$Minerio, mydata$SubSubExp,
                            mydata$Dose1,mydata$Aditivo1,
                            mydata$Dose2,mydata$Aditivo2,
                            mydata$Dose3,mydata$Aditivo3,
                            mydata$Dose4,mydata$Aditivo4,
                            mydata$Dose5,mydata$Aditivo5,
                            mydata$AB))
# Verificar o Número de n??veis de IDS
str(mydata$ID)

mydata$Amostra = as.factor(paste(mydata$Minerio,mydata$TS,'\n',
                                 mydata$Dose1,mydata$Aditivo1,'\n',
                                 mydata$Dose2,mydata$Aditivo2,'\n',
                                 mydata$Dose3,mydata$Aditivo3,'\n',
                                 mydata$Dose4,mydata$Aditivo4,'\n',
                                 mydata$Dose5,mydata$Aditivo5,'\n',
                                 mydata$Camadas))


# Remover espacos no final e excesso no meio 
mydata$Amostra = as.factor(trimws(mydata$Amostra, which = c('right')))

# mydata$Amostra = as.factor(gsub("\\s+"," ",mydata$Amostra))

levels(mydata$Amostra)

# Continuar criando features

mydata$Aditivo = as.factor(paste(mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3,
                                 mydata$Dose4,mydata$Aditivo4,
                                 mydata$Dose5,mydata$Aditivo5))


# EU uso essa "Aditivos", se sim, fudeu pq s? tem dois valores
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

# Verificar quais Experimentos està£o dispon??veis
levels(mydata$Experimento)

# Nà£o vamos trabalhar com Choque Térmico, nem Cura UV
levels(mydata$SubExp)

# Sem esse filtro por equanto
# mydata = subset(mydata, (SubExp %in% c('Verde','Seca','Curada UV')))


table(mydata$SubExp)
table(mydata$Amostra)
table(mydata$SubSubExp)

# Nà£o vamos trabalhar com TS que nà£o seja PFM ou 1 mm
mydata = subset(mydata, (TS %in% c('1 mm', '')))

# Criar as fam??lias -------------------------------------------------------
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
      str_detect(Aditivo1, "Lama") ~ "Lama",
      str_detect(Aditivo1, "PFM") ~ "PFM",
      str_detect(Aditivo2, "Per") ~ "Peridur", 
      str_detect(Aditivo1, "Per") ~ "Peridur",
      str_detect(Aditivo1, "PEO") ~ "Amido ou PEO",
      str_detect(Aditivo1, "Amido") ~ "Amido ou PEO",
      str_detect(Aditivo1, "Arkomon") ~ "Arkomon ou TPP",
      str_detect(Aditivo2, "Arkomon") ~ "Arkomon ou TPP",
      str_detect(Aditivo1, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivo2, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivo1, "BN") ~ "Referências",
      TRUE ~ Familia
    )
  )
table(mydata_new$Familia)
mydata_new$Familia = as.factor(mydata_new$Familia)
levels(mydata_new$Familia)
table(mydata_new$Familia)

table(mydata_new$Amostra)

# Apà³s o processo, voltar para o nome de objeto mydata
mydata = mydata_new
# Para poder trabalhar com porosidade e outras varià¡vieis em outros scripts, nà£o tirar ainda
subdata2 = (subset(mydata, 
                   (TS == "1 mm") |
                     (Minerio == "PFM" & Aditivo1 == "BN")))

# Continuar a analise desses boxplots
subdata = (subset(mydata, 
                  (Experimento %in% c('Resistencia') & TS == "1 mm") |
                    (Experimento %in% c('Resistencia') & Minerio == "PFM" & Aditivo1 == "BN")))

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

#  Ordem das amostras para apresentaà§à£o no boxplot ------------------------
# Ordem das amostras
levels(subdata$Amostra)
table(subdata$Amostra)

# Remover n??veis com contagem zero 
?droplevels
subdata$Amostra = droplevels(subdata$Amostra)

levels(subdata$Amostra)

# Remover espaà§os no final
# subdata$Amostra = as.factor(trimws(subdata$Amostra, which = c('right')))
# levels(subdata$Amostra)

subdesgraca = subdata
# 
# #Depois mexemos com isso dos nomes ---------------------------------------
# # Deixar BN para depois nos nomes, na ra?a
# #E arrumar os nomes dos de lama tamb?m 
# vetorpattern = c("PFNM 1 mm 0.50% BN 0.04% TPP",
#                  "PFNM 1 mm 0.50% BN 0.06% Per 0.02% NaOH",
#                  "PFNM 1 mm 0.50% BN 0.04% Arkomon",
#                  "PFNM 1 mm 0.50% BN 0.06% Per 0.04% Arkomon",
#                  "PFNM 1 mm 0.50% BN 0.06% Per",
#                  "PFNM 1 mm 1.00% BN 0.06% Per 0.02% NaOH",
#                  "PFNM 1 mm 0.70% BN 0.06% Per 0.02% NaOH",
#                  "PFNM 1 mm 0.35% BN 0.06% Per 0.02% NaOH")
# 
# vetorreplace = c("PFNM 1 mm 0.04% TPP 0.50% BN",
#                  "PFNM 1 mm 0.06% Per 0.02% NaOH 0.50% BN",
#                  "PFNM 1 mm 0.04% Arkomon 0.50% BN",
#                  "PFNM 1 mm 0.06% Per 0.04% Arkomon 0.50% BN",
#                  "PFNM 1 mm 0.06% Per 0.50% BN",
#                  "PFNM 1 mm 0.06% Per 0.02% NaOH 1.00% BN",
#                  "PFNM 1 mm 0.06% Per 0.02% NaOH 0.70% BN",
#                  "PFNM 1 mm 0.06% Per 0.02% NaOH 0.35% BN")
# 
# levels(subdata$Amostra)
# 
# for (k in 1:length(vetorpattern)){
#   subdata$Amostra = as.factor(str_replace(subdata$Amostra,
#                                           vetorpattern[k],
#                                           vetorreplace[k]))
#   print(vetorpattern[k])
#   print(vetorreplace[k])
# }
# 
# levels(subdata$Amostra)
# 
# # Deixar na ordem desejada  
# samplelist = c("PFM 0.50% BN",
#                "PFNM 1 mm 0.50% BN",
#                "PFNM 1 mm 10.00% PFM 0.06% Per 0.02% NaOH 0.70% BN",
#                "PFNM 1 mm 5.00% Lama 0.06% Per 0.02% NaOH 0.70% BN 0.04% Arkomon",
#                "PFNM 1 mm 5.00% Lama 0.70% BN",
#                "PFNM 1 mm 5.00% Lama 0.02% NaOH 1.50% BN",
#                "PFNM 1 mm 5.00% Lama 0.06% Per 0.02% NaOH 0.70% BN",
#                "PFNM 1 mm 5.00% Lama 0.06% Per 0.02% NaOH 0.50% BN",
#                "PFNM 1 mm 5.00% Lama 0.06% Per 0.02% NaOH 0.35% BN",
#                "PFNM 1 mm 10.00% Lama 0.06% Per 0.02% NaOH 0.70% BN",
#                "PFNM 1 mm 10.00% Lama 0.06% Per 0.02% NaOH 0.70% BN Camadas",
#                "PFNM 1 mm 0.04% TPP 0.50% BN",	
#                "PFNM 1 mm 0.15% TPP", 
#                "PFNM 1 mm 0.04% TPP",
#                "PFNM 1 mm 0.04% Arkomon 0.50% BN",                                           
#                "PFNM 1 mm 0.15% Arkomon",  
#                "PFNM 1 mm 0.04% Arkomon", 
#                "PFNM 1 mm 0.12% PEO",                                           
#                "PFNM 1 mm 0.04% PEO",
#                "PFNM 1 mm 0.12% Amido", 
#                "PFNM 1 mm 0.04% Amido", 
#                "PFNM 1 mm 0.12% Per 0.02% NaOH", 
#                "PFNM 1 mm 0.04% Per 0.02% NaOH",
#                "PFNM 1 mm 0.06% Per 0.04% Arkomon 0.50% BN",
#                "PFNM 1 mm 0.06% Per 0.50% BN",
#                "PFNM 1 mm 0.06% Per 0.02% NaOH 1.00% BN",
#                "PFNM 1 mm 0.06% Per 0.02% NaOH 0.70% BN",
#                "PFNM 1 mm 0.06% Per 0.02% NaOH 0.50% BN",
#                "PFNM 1 mm 0.06% Per 0.02% NaOH 0.35% BN")
# size(samplelist)
# subdata$Amostra = ordered(subdata$Amostra, 
#                           levels = samplelist)
# subdata$Amostra = fct_rev(subdata$Amostra)
# levels(subdata$Amostra)
# 
# table(subdata$Amostra)

#Substituir os c?digos A e B pelos tamanhos das pelotas reais
subdata$AB = sub("A","-16 mm +12,5 mm",subdata$AB)
subdata$AB = sub("B","-12,5 mm +9,5 mm",subdata$AB)
#Remove the NAs returned (valores nulos que n?o existem)

# Para facilitar a iteraà§à£o nos experimentos com apenas uma coluna
subdata$Exp = as.factor(paste(subdata$Experimento,subdata$SubExp))
levels(subdata$Exp)
str(subdata)
levels(subdata$Exp)

subdata = subdata[subdata$AB %in% c('-12,5 mm +9,5 mm'),]
subdata = subdata[!subdata$TempoUV %in% c('24h'),]

#Para poder agrupar por SubSubExp que cont?m a especifica??o de temperaturas do choque t?rmico
medias = subdata %>%
  group_by(Ensaio, SubExp, SubSubExp, TS, AB, Amostra, TempoUV) %>%
  summarise_at(vars(Valor), list(name = mean))

nft = length(medias)
names(medias)[nft] = 'media'
medias[nft] = round(medias[nft], 2)

subdata$AB = as.factor(subdata$AB)
levels(subdata$AB)

levels(subdata$Amostra)

g = ggplot(subdata, aes(x = reorder(Amostra, desc(Amostra)), 
                        y = Valor)) + 
  geom_boxplot(aes(color = SubExp, fill = SubExp), alpha = 0.4, outlier.size = 0.8) + 
  # coord_flip() + 
  geom_text(data = medias, aes(group = SubExp, label = media, y = media),size =4,
            position = position_dodge(0.8)) +
  # geom_hline(yintercept = vline, color = 'red', linetype = 'dashed', size = 1) + 
  xlab("") + 
  # ylab(legenda[j,2]) + #axisnames do dataframe legenda 
  # ggtitle(paste(legenda[j,3],'das pelotas de granulometria',granulist[i])) + d
  # scale_color_manual(name = 'Família', values =  cores) + 
  # scale_fill_manual(name = 'Família', values =  cores) + 
  # scale_fill_discrete(name = 'Fam??lia') + 
  theme  
g

# 
# path = 'C:/Users/vpb/IPT/Francisco Junior Batista Pedrosa (FIPT) - Aglomeração de grossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS/outputdata/plots/Boxplots/NOVA_REFERENCIA_46/Res'
# tiff(filename, units = 'in', width = w, height = h, res = res)
# dev.off()
# }
# }

