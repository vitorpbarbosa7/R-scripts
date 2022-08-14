setwd('C:/Users/vitor/IPT/Francisco Junior Batista Pedrosa (FIPT) - AgloGrossos/!Ana/Pelotizacao/Boxplot/R/MEDIAS')
rm(list=ls())

# Pacotes que serão utilizados
library(tidyvers)
library(lemon)
library(ggrepel)
library(plotly)

# Carregar o dataset criado com o Script "Plots_DuasGranulometriasSimultaneaas_v2.R"
load(file = 'datasets/df.Rdata')

# Verificar que sua estrutura de mantém
# Muito melhor fazer assim do que salvar em um csv no qual toda codificação dos tipos de dados seriam perdidos
str(df)

# Gravar csv apenas com quedas, verde e seca
csv = df 
csv[,c(14:23,25,27)] = NULL
?write.csv
write.csv(csv, 'datasets/csv/quedasverdeseca.csv', row.names = FALSE)

# Para o plot -------------------------------------------------------------

# Cores
coresgranu = c('-16+12,5 mm' = '#3540f3',
               '-12,5+9,5 mm' = '#FF0000')

coresref = c('Aglomerantes e/ou Aditivos em estudo' = '#3540f3',
                      'Amostras de referência com 0,5 % Bentonita Nacional' = '#FF0000')

# Tema
theme = theme_bw(base_size = 15) + 
  theme(legend.position = 'right',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12), #Posi??o da legenda
        plot.title = element_text(hjust = +.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), #Posi??o do t?tulo
        axis.line = element_line(colour = "black"),
        axis.text.x=element_text(colour="black", size = 16),
        axis.text.y=element_text(colour="black", size = 16), #Cor do texto dos eixos
        strip.background =element_rect(fill=NA, colour = NA)) #Cor do background dos t?tulos de cada face


# Verde x Seca e Quedas por tamanho
gfamilyall = ggplot(df, aes(x = Resistencia_Seca, y = Resistencia_Verde, size = Quedas_)) + 
  geom_point(aes(shape = Amostra, alpha = AB, color = Referencia)) + 
  # scale_size(range = c(5,12)) +
  scale_size_continuous(name = 'Número de Quedas', range = c(2,8)) + 
  scale_shape_manual(name = '', values = c(1:14,35:38,17,19)) + 
  scale_alpha_manual(name = 'Granulometria das pelotas', values = c(1,0.5)) + 
  scale_color_manual(name = '', values =  coresref) + 
  geom_text_repel(aes(label = Quedas_, size = Quedas_), force = 5) + 
  xlab('Resistência à compressão seca (kgf/pelota)') + 
  ylab('Resistência à compressão verde (kgf/pelota)') + 
  facet_rep_wrap(.~Familia, repeat.tick.labels = TRUE, nrow = 2, ncol = 2) +
  guides(shape = guide_legend(ncol = 1, order = 4, override.aes = list(size = 5)),
         color = guide_legend(order = 1, override.aes = list(size = 5)),
         alpha = guide_legend(order = 3, override.aes = list(size = 5)),
         size = guide_legend(order = 2)) + 
  theme

gfamilyall
# ggplotly

tiff('outputdata/plots/Conclusao/all.tiff', width = 16, height = 10, units = 'in', res = 120)
gfamilyall
dev.off()

# Quedas com Verde e seca por tamanho
df$Resistencia_Seca = round(df$Resistencia_Seca,2)

gquedasverde = ggplot(df, aes(x = Resistencia_Verde, y = Quedas_, size = Resistencia_Seca)) + 
  geom_point(aes(shape = Amostra, alpha = AB, color = Referencia)) + 
  scale_size_continuous(name = 'Resistência à compressão seca (kgf/pelota)', range = c(2,6)) + 
  scale_shape_manual(name = '', values = c(1:14,35:38,17,19)) + 
  scale_alpha_manual(name = 'Granulometria das pelotas', values = c(1,0.5)) + 
  scale_color_manual(name = '', values =  coresref) + 
  geom_text_repel(aes(label = Resistencia_Seca, size = Resistencia_Seca), force = 5) + 
  xlab('Resistência à compressão verde (kgf/pelota)') + 
  ylab('Número de Quedas (-)') + 
  facet_rep_wrap(.~Familia, repeat.tick.labels = TRUE, nrow = 2, ncol = 2) +
  guides(shape = guide_legend(ncol = 1, order = 4, override.aes = list(size = 5)),
         color = guide_legend(order = 1, override.aes = list(size = 5)),
         alpha = guide_legend(order = 3, override.aes = list(size = 5)),
         size = guide_legend(order = 2)) + 
  theme

gquedasverde
tiff('outputdata/plots/Conclusao/quedasverde.tiff', width = 16, height = 10, units = 'in', res = 120)
gquedasverde
dev.off()

# Subset com as melhores amostras -----------------------------------------

# Nova coluna de ID numérico de acordo com nome da amostra
df$ID = as.integer(df$Amostra)

df_sub = subset(df, ID %in% c(1,2,3,4,5,6,20,19))

# Remover as outras famílias para as referências
df_sub = subset(df_sub, !(ID %in% c(19,20) & Familia %in% c('Arkomon ou TPP','Amido ou PEO')))

medias = df_sub %>%
  select(ID, Quedas_, Resistencia_Verde, Resistencia_Seca) %>%
  group_by(ID) %>%
  summarise(media_quedas = mean(Quedas_),
            media_verde = mean(Resistencia_Verde),
            media_seca = mean(Resistencia_Seca))

mquedas_ref = as.numeric(round(medias[medias$ID == 20, 2],2))
mverde_ref = as.numeric(round(medias[medias$ID == 20, 3],2))
mseca_ref = as.numeric(round(medias[medias$ID == 20, 4],2))

df_sub$QuedasBool = ifelse(df_sub$Quedas_ >= mquedas_ref,
                           'Número de Quedas superior à referência',
                           'Número de Quedas inferior à referência')

# 4 Cores
coresref_melhores = c('Aglomerantes e/ou Aditivos em estudo' = '#3540f3',
                      'Amostras de referência com 0,5 % Bentonita Nacional' = '#FF0000',
                      'Número de Quedas superior à referência' = '#12a16c', 
                      'Número de Quedas inferior à referência' = '#FFA500')

gseis = ggplot(df_sub, aes(x = Resistencia_Seca, y = Resistencia_Verde, size = Quedas_)) + 
  geom_point(aes(shape = Amostra, alpha = AB, color = Referencia), stroke = 2) + 
  # scale_size(range = c(5,12)) +
  scale_size_continuous(name = 'Número de Quedas', range = c(2,8)) + 
  scale_shape_manual(name = '', values = c(1:6,17,19)) + 
  scale_alpha_manual(name = 'Granulometria das pelotas', values = c(1,0.5)) + 
  scale_color_manual(name = '', values =  coresref_melhores) + 
  geom_text_repel(aes(label = Quedas_, size = Quedas_, color = QuedasBool), force = 9) + 
  geom_hline(yintercept = mverde_ref, linetype = 'dashed', size = 1.2, color = 'red') + 
  geom_vline(xintercept = mseca_ref, linetype = 'dashed', size = 1.2, color = 'red') + 
  xlab('Resistência à compressão seca (kgf/pelota)') + 
  ylab('Resistência à compressão verde (kgf/pelota)') + 
  # facet_rep_wrap(.~Familia, repeat.tick.labels = TRUE, nrow = 2, ncol = 2) +
  guides(shape = guide_legend(ncol = 1, order = 4, override.aes = list(size = 5)),
         color = guide_legend(order = 1, override.aes = list(size = 5)),
         alpha = guide_legend(order = 3, override.aes = list(size = 5)),
         size = guide_legend(order = 2)) + 
  theme

gseis

tiff('outputdata/plots/Conclusao/melhores.tiff', width = 14, height = 8, units = 'in', res = 120)
gseis
dev.off()






