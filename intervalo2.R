#Carregando pacote
library(tidyverse)

#Lendo arquivo
jogos_df<-read.csv("games.csv")

#variaveis de teste
#jogos_df<- jogos_df[jogos_df$Genre == "Puzzle",]
jogos_df$NA_Sales<- jogos_df$NA_Sales*100
teste1 <-na.omit(jogos_df[,c(4,6)])
names(teste1)<-c("categoria","vendas" )


#Função que faz ic para uma amostra
ic_medias <- function(x,alpha){
  teste_poisson<-function(y,categoria){
    resultado_teste<-poisson.test(x= sum(y) ,T= length(y),r = mean(y) ,conf.level =  alpha)
    retorno<-list(a=resultado_teste$conf.int[1],b=resultado_teste$conf.int[2],c=mean(y),d=categoria)
    return(retorno)
  }
  resultados<- map(unique(x$categoria), function(y) teste_poisson(x[x$categoria==y,"vendas"], y))
  resultados_lista <- map(resultados,\(x) as.data.frame(x))
  resultados_df <- list_rbind(resultados_lista)
  return(resultados_df)
} 


#Lembrete deixar o gráfico mais bonito no momento está horrorso
teste2<-ic_medias(teste1, 0.95)
names(teste2)<-c("intervalo inferior","intervalo superior", "medias", "categoria" )
ggplot(aes(y=medias , x =categoria ) ,data=teste2) +geom_point()+ geom_point(aes(y=`intervalo inferior`),color="pink") + 
  #geom_point(aes(y=`intervalo superior`), color="green") + theme_minimal() 
  ggplot(aes(y=medias , x =categoria ) ,data=teste2) +geom_point()+ geom_errorbar(aes(ymax=`intervalo superior`, ymin=`intervalo inferior`))




###########  teste     ############################
teste2<-ic_medias(teste1, 0.95)
unique(teste1$categoria)
hist(teste1$categoria)
teste1[teste1$categoria ==  "Platform", ]

[1] "Sports"       "Platform"     "Racing"       "Role-Playing" "Puzzle"       "Misc"         "Shooter"     
[8] "Simulation"   "Action"       "Fighting"     "Adventure"    "Strategy" 

#Plotando esse valores