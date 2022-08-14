rm(list=ls())
setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
library(tidyverse)
library(lemon) # para facet_rep_wrap

# Carregar os dados -------------------------------------------------------
data = read.csv('indata/Flutuado.csv')

# mydata ------------------------------------------------------------------
mydata = data

mydata$Amostra = as.factor(paste(mydata$Minerio,mydata$TS,
                                 mydata$Dose1,mydata$Aditivo1,
                                 mydata$Dose2,mydata$Aditivo2,
                                 mydata$Dose3,mydata$Aditivo3))
levels(mydata$Amostra)

# Criar as famílias -------------------------------------------------------
mydata$Aditivos = as.factor(paste(mydata$Aditivo1, mydata$Aditivo2, mydata$Aditivo3))
levels(mydata$Aditivos)
mydata$Familia1 = as.character(mydata$Aditivos)
levels(mydata$Aditivos)
# Familia Bentonita e Dispersantes dispersantes

library(stringr)

# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
mydata_new = 
  mydata %>%
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

mydata_new$Familia1 = as.factor(mydata_new$Familia1)
levels(mydata_new$Familia1)

# Familia 2 
# SEMPRE SALVAR O RESULTADO DE MUTATE, DPLYR COM %IN% EM UM NOVO OBJETO
mydata_new$Familia2 = as.character(mydata$Aditivos)

levels(mydata_new$Familia2)

mydata_new = 
  mydata_new %>%
  mutate(
    Familia2 = case_when(
      str_detect(Aditivos, "Bentonita Nacional") ~ "Bentonita Nacional",
      str_detect(Aditivos, "Arkomon") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "TPP") ~ "Arkomon ou TPP",
      str_detect(Aditivos, "PEO") ~ "Amido ou PEO",
      str_detect(Aditivos, "Amido") ~ "Amido ou PEO",
      str_detect(Aditivo1, "Peridur") ~ "Peridur",
      TRUE ~ Familia2
    )
  )
mydata_new$Familia2 = as.factor(mydata_new$Familia2)
levels(mydata_new$Familia2)

mydata = mydata_new
# Arrumar a ordem dos fatores das amostras
# Remover espaços no final
mydata$Amostra = as.factor(trimws(mydata$Amostra, which = c('right')))
levels(mydata$Amostra)

mydata$Amostra = ordered(mydata$Amostra, 
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
mydata$Amostra = fct_rev(mydata$Amostra)
levels(mydata$Amostra)

# Plot --------------------------------------------------------------------
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18), #Posi??o da legenda
        plot.title = element_text(hjust = +.5), #Posi??o do t?tulo
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour="black", size = 22),
        axis.text.y=element_text(colour="black", size = 18), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face

require(scales)
# Gerar o gr?fico
ggplot(data = mydata, aes(x = Tamanho, y = Valores)) + 
  geom_line(aes(color = Amostra), size = 1) + 
  facet_rep_wrap(.~Familia1, repeat.tick.labels = TRUE) + 
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                # labels = scales::trans_format("log10", scales::math_format(10^.x))
  # ) +
  scale_x_log10(labels = comma) + 
  theme







