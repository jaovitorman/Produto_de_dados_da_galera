#código super legal para templates 
#Lembrando que no nosso leitor yaml
#----
#params: 
#  nome_do_parametro: "parametro defaut" 
#---




#Biblioteca usadas
library(DAAG)

##vamos primeiro criar a lista de parametros que desejamos
parametros <-c("northing","easting","altitude")

##Vamos fazer um para cada umas das regressoras.

multiplos_relatorios<- function(parametro){
  template<- "sapos.Rmd"
  outfile<-paste0("regressao com ",parametro)
             parametros<-list(frog = parametro)
             rmarkdown::render(template,
                               output_file = outfile,
                               params = parametros
             )
             invisible(TRUE)
}

#Para multiplos parametros podemos usar o purr
purrr::walk(parametros, multiplos_relatorios)
