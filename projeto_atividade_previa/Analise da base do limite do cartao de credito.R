library(readxl)
library(mice)

cartao_credito <- data.frame(read_excel("D:/Estudos/MBA - Engenharia de Dados/CPD - Coleta e Preparação de Dados/Trabalho Pratico/default of credit card clients.xls",col_names = TRUE, skip = 1))

#Verificando dados ausentes na base geral
md.pattern(cartao_credito)

head(cartao_credito)

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
  ,IDADE = c(cartao_credito$AGE)
  ,DS_IDADE = cut(cartao_credito$AGE, breaks = c(0,12,18,60,100), 
                 labels = c("crianca","adolescente","adulto","idoso"))
  ,LIMITE = c(cartao_credito$LIMIT_BAL)
  )

base_origem <- analise

head(analise)
tail(analise)

#verifica dados ausentes na base de analise
md.pattern(analise)

#Retirando dados ausentes
analise <- analise[complete.cases(analise), ]

md.pattern(analise)

head(analise)

#realizando normalizacao em limiite por z-score
analise$LIMITE <- scale(analise$LIMITE)

head(analise)

#indetificando outliers
summary(analise)

iqr_lim <- IQR(analise$LIMITE)
iqr_lim

li_lim <- -0.905483 - 1.5*iqr_lim
ls_lim <- 0.558898 + 1.5*iqr_lim

data.frame(which(analise$LIMITE < li_lim))
data.frame(which(analise$LIMITE > ls_lim))

data.frame(analise$LIMITE[which(analise$LIMITE < li_lim)])

#identificando outlier com cooks
modelo <- lm(analise$LIMITE~., data = analise)
distancia <- cooks.distance(modelo)
plot(distancia, pch = "*", cex = 2, main = "influencia sengudo a distancia LIMITE")
abline(h = 4 * mean(distancia, na.rm = T), col = "red")

#separando os elementos influentes
influentes <- as.numeric(names(distancia)[(distancia > 4 * mean(distancia, na.rm = T))])
influentes

#verficando na base quem sao eles
head(analise[influentes, ])

#retirando outlier da analise
analise$LIMITE[analise$LIMITE > ls_lim] = ls_lim

summary(analise$LIMITE)

boxplot(analise$LIMITE)

head(analise)



summary(analise)

modelo = lm(analise$LIMITE_NORM~.,data = analise)
distancia = cooks.distance(modelo)
plot(distancia, pch="*", cex=2, main="Influencia segundo a distancia")
abline(h=4*mean(distancia,na.rm = T), col="red")

influentes <- as.numeric(names(distancia)[(distancia > 4*mean(distancia, na.rm = T))])
influentes

