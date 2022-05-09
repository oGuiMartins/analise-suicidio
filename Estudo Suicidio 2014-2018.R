library("MASS")
library("rpart")
library("ggplot2")
library ("tidyverse")
library("randomForest")
library ("lubridate") #Trabalhar com Datas
library ("caret")

set.seed(7)

DF <- read.csv("datasus_suicidio_2014_2018.csv")

#Inspeção de Dados

DF %>% glimpse

#Passando chr para date
DF$DTOBITO <- dmy(DF$DTOBITO)
DF$DTNASC <- dmy(DF$DTNASC)

#Analise anual
ggplot(data = DF,aes(x=ano)) +
  geom_bar(fill="rosybrown2")

#Analise por Estado  
ggplot(data = DF,aes(x=estado)) +
  geom_bar(fill="rosybrown2")

#analise por idade
ggplot(data = DF,aes(x=idade)) +
  geom_histogram(fill="rosybrown2",color="white",binwidth = 5,)+
  ylab("") + xlab("Idade")+ ggtitle("Análise por idade") 
  

#analise por estado x idade
ggplot(data = DF,aes(x=idade)) +
  geom_histogram(fill="rosybrown2",color="white")+
  ggtitle("Análise por idade") + facet_wrap(~ estado, ncol = 7)

#Analise por mes
ggplot(data = DF,aes(x=mes)) +
  geom_histogram(fill="rosybrown2",color="white",binwidth = 1,)+
  ylab("") + xlab("Mês")+ ggtitle("Análise por mês")

#analise por mes x idade
ggplot(data = DF,aes(x=idade)) +
  geom_histogram(fill="rosybrown2",color="white")+
  ggtitle("Análise por mes x idade") + facet_wrap(~ mes, ncol = 7)

#analise estado x mes
ggplot(data = DF,aes(x=mes)) +
  geom_histogram(fill="rosybrown2",color="white")+
  ggtitle("Análise por mes x estado") + facet_wrap(~ estado, ncol = 7)

#Removi NA mas deixei o dataframe original

DFp <- na.omit(DF)
DFp$DTNASC <- NULL
DFp$DTOBITO <- NULL
DFp$CODMUNRES <- NULL #demora muito pra carregar
DFp$CAUSABAS_O <- NULL #duplicado
DFp$OCUP <- NULL #demora muito pra carregar

#Criar ID para cada linha

id <- seq.int (nrow(DFp)) 
DFp <- cbind(id,DFp)

#Trasformar em Dummys 
glimpse(DFp)

library(data.table)

  #estado
DFp_estado <- as.data.table(DFp)
DFp_estado <- dcast(DFp, id ~ estado, fun.aggregate = function(x)1, value.var= 'estado',fill=0)
DFp <- left_join(DFp,DFp_estado)

  #RACACOR
DFp_racacor <- as.data.table(DFp)
DFp_racacor <- dcast(DFp, id ~ RACACOR, fun.aggregate = function(x)1, value.var= 'RACACOR',fill=0)
DFp <- left_join(DFp,DFp_racacor)

  #ESTCIV
DFp_ESTCIV <- as.data.table(DFp)
DFp_ESTCIV <- dcast(DFp, id ~ ESTCIV, fun.aggregate = function(x)1, value.var= 'ESTCIV',fill=0)
DFp <- left_join(DFp,DFp_ESTCIV)

  #ESC
DFp_ESC <- as.data.table(DFp)
DFp_ESC <- dcast(DFp, id ~ ESC, fun.aggregate = function(x)1, value.var= 'ESC',fill=0)
DFp <- left_join(DFp,DFp_ESC)

  #OCUP
#DFp_OCUP <- as.data.table(DFp)
#DFp_OCUP <- dcast(DFp, id ~ OCUP, fun.aggregate = function(x)1, value.var= 'OCUP',fill=0)
#DFp <- left_join(DFp,DFp_OCUP)

  #LOCOCOR
DFp_LOCOCOR <- as.data.table(DFp)
DFp_LOCOCOR <- dcast(DFp, id ~ LOCOCOR, fun.aggregate = function(x)1, value.var= 'LOCOCOR',fill=0)
DFp <- left_join(DFp,DFp_LOCOCOR)

#ASSISTMED
DFp_ASSISTMED <- as.data.table(DFp)
DFp_ASSISTMED <- dcast(DFp, id ~ ASSISTMED, fun.aggregate = function(x)1, value.var= 'ASSISTMED',fill=0)
DFp <- left_join(DFp,DFp_ASSISTMED)

#CAUSABAS 
DFp_CAUSABAS  <- as.data.table(DFp)
DFp_CAUSABAS  <- dcast(DFp, id ~ CAUSABAS, fun.aggregate = function(x)1, value.var= 'CAUSABAS',fill=0)
DFp <- left_join(DFp,DFp_CAUSABAS)

#Retirando as originais


DFp$estado <- NULL
DFp$RACACOR <- NULL
DFp$ESTCIV <- NULL
DFp$ESC <- NULL
DFp$OCUP <- NULL
DFp$LOCOCOR <- NULL
DFp$ASSISTMED <- NULL
DFp$CAUSABAS <- NULL
DFp$CODMUNRES<- NULL
DFp$CIRCOBITO<- NULL

#alterando a posição da variavel alvo

library(dplyr)

DFp <- DFp %>% relocate(SEXO,.after=Y870)
DFp
DFp <- na.omit(DFp)

#Sexo

DFp$SEXO[DFp$SEXO=="Masculino"] <- 1
DFp$SEXO[DFp$SEXO=="Feminino"] <- 0
DFp$SEXO <- as.numeric(DFp$SEXO)


#Benchmarkv - Arvore

train_test_split_index <- 0.8*nrow(DFp)
train <- data.frame(DFp[1:train_test_split_index,])
test <- data.frame(DFp[(train_test_split_index+1):nrow(DFp),])

fit_tree <- rpart(SEXO ~.,method="anova", data=train)
tree_predict <- predict(fit_tree, test)                 
mse_tree <- mean((tree_predict - test$SEXO)^2)                 

#Neural Network

library(neuralnet)

#nn <- neuralnet(SEXO~idade+mes+AC+AL+AM+AP+BA+CE+DF+ES+GO+MA+MG+MS+MT+PA+PB+PE+PI+PR+RJ+RN+RO+RR+RS+SC+
                  #SE+SP+TO+Amarela+Branca+Indígena+Parda+Preta+Casado+Separado judicialmente+Solteiro+
                  #'União consensual'+Viúvo+ '1 a 3 anos'+'12 e mais'+'4 a 7 anos'+'8 a 11 anos+Nenhuma'+ 
                  #6+Domicílio+Hospital+'Outro estabelecimento de saúde'+Outros+'Via pública'+Não,data = train, 
                  #hidden= c(5,4,3,2), linear.output = TRUE)

nn <- neuralnet(SEXO~.,data = train,hidden= c(2,3), act.fct = "logistic", linear.output = TRUE)


head(test[310])
pr.nn <- compute(nn,test[,1:309])
MSE_nn <- mean((pr.nn - test)^2)

pr.nn$net.result <- sapply(pr.nn$net.result,round,digits=2)
pr.nn$net.result

class(pr.nn)


