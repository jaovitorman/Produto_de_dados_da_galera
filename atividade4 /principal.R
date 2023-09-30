library(yaml)

#Caso 1
library(revisao199910)

#Caso 2
#Dado que o nosso arquivo tar.gz já está na pasta do projeto
devtools::install_local("revisao199910_0.1.0.tar.gz")
library(revisao199910)

configuracao<-read_yaml("config.yaml")
hist_ou_densi(iris,configuracao[[4]]$variavel_iris,configuracao[[5]]$tipografico)


