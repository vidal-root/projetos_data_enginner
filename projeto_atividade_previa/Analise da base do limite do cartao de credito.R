library(readxl)
library(mice)
library(httr)
library(jsonlite)

setwd("C:/Users/vidal_root/Documents/Estudo/MBA - Data Enginner/Coleta e Preparação dos Dados/Atividades/projeto_atividade_previa")

source(file = 'functions.R')

cartao_credito <- data.frame(read_excel("default of credit card clients.xls",col_names = TRUE, skip = 1))

#Verificando dados ausentes na base geral
md.pattern(cartao_credito)

#excluir colunas
View(cartao_credito)
idx_colunm_excluir <- c(7:24)
cartao_credito <- cartao_credito[,-idx_colunm_excluir]
View(cartao_credito)

#conversão para real
nr_dollar = as.numeric(get_nr_dolar())
cartao_credito$LIMIT_BAL <- cartao_credito$LIMIT_BAL * nr_dollar
View(cartao_credito)

#separa dados de analise
analise <- data.frame(
  SEXO = cut(cartao_credito$SEX
             , breaks = c(0,1,2)
             , labels = c("M","F"))
  ,EDUCACAO = cut(cartao_credito$EDUCATION
                  , breaks = c(0,1,2,3,4)
                  , labels = c("pos-graduacao","universidade","ensino medio","outros"))
  ,ESTADO_CIVIL = cut(cartao_credito$MARRIAGE
                      , breaks = c(0,1,2,3)
                      , labels = c("casado","solteiro","outros"))
  #,IDADE = c(cartao_credito$AGE)
  ,DS_IDADE = cut(cartao_credito$AGE, breaks = c(0,12,18,60,100), 
                  labels = c("crianca","adolescente","adulto","idoso"))
  ,LIMITE = c(cartao_credito$LIMIT_BAL)
  ,DS_LIMITE = cut(cartao_credito$LIMIT_BAL
                   , breaks = c(0,49999,99999,499999,99999999)
                   , labels = c("silver","gold","platium","black")
                   
  )
  ,PROPENS = c(cartao_credito$default.payment.next.month)
)


#verifica dados ausentes na base de analise
md.pattern(analise)

#Retirando dados ausentes
analise_sem_ausentes <- analise[complete.cases(analise), ]

md.pattern(analise_sem_ausentes)

dim(analise_sem_ausentes)

#realizando normalizacao em limiite por z-score
analise_sem_ausentes$LIMITE_NORM <- scale(analise_sem_ausentes$LIMITE)

head(analise_sem_ausentes)

#indetificando outliers

summary(analise_sem_ausentes) #pegar o 1Q e 3Q para o calculo de limites

IQR(analise_sem_ausentes$LIMITE_NORM) 

li <- -0.904624 - 1.5*IQR(analise_sem_ausentes$LIMITE_NORM)
li
ls <- 0.557544 + 1.5*IQR(analise_sem_ausentes$LIMITE_NORM)
ls

boxplot(analise_sem_ausentes$LIMITE_NORM) #boxplot considerando  os outliers

#dados que estao abaixo do limite inferior
data.frame(analise_sem_ausentes$LIMITE_NORM[which(analise_sem_ausentes$LIMITE_NORM < li)])
#dados que estao acima  do limite superior
data.frame(analise_sem_ausentes$LIMITE_NORM[which(analise_sem_ausentes$LIMITE_NORM > ls)])

#filtrando os dados que estao acima do limite superior (outlier)
outlier_ls <- filter(analise_sem_ausentes, LIMITE_NORM >ls )

#retirandoo os outlier
analise_sem_ausentes_e_outlier <- filter(analise_sem_ausentes, LIMITE_NORM <= ls )

boxplot(analise_sem_ausentes_e_outlier$LIMITE_NORM)

dim(analise_sem_ausentes_e_outlier)

summary(analise_sem_ausentes_e_outlier)


#identificando outlier com cooks (considerando multivariaveis)
modelo <- lm(analise_sem_ausentes$LIMITE_NORM~., data = analise_sem_ausentes)
distancia <- cooks.distance(modelo)
plot(distancia, pch = "*", cex = 2, main = "influencia sengudo a distancia")
abline(h = 4 * mean(distancia, na.rm = T), col = "red")

#separando os elementos influentes
influentes <- as.numeric(names(distancia)[(distancia > 4 * mean(distancia, na.rm = T))])
influentes

#verficando na base quem sao eles
head(analise_sem_ausentes[influentes, ])

summary(analise_sem_ausentes)

boxplot(analise$LIMITE)

head(analise)


