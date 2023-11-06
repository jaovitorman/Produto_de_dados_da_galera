# Projeto API

#Lendo arquivo
jogos_df<-read.csv("games.csv")

#variaveis de teste
vendas_df<-na.omit(jogos_df[,6:9])
x<-"JP_Sales"
y<-"NA_Sales"

regressao<- function(x,y){
  f <- as.formula(
        paste(y, 
        paste(x, collapse = " + "), 
        sep = " ~ "))
  fit<-glm(f,family = poisson(), data = vendas_df)
  return(fit$residuals)
  }


