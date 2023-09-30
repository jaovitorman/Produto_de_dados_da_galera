#' @title Histograma ou Densidade
#' @import ggplot2
#' @param df Um dataframe .
#' @param variavel Uma string indicando qual e a coluna de df a ser plotada
#' @param tipo O tipo do grafico a ser plotado
#'
#'
#' @return Retorna um grafico do tipo densidade ou histograma
#' @examples  hist_ou_densi(iris,"Sepal.Length","densidade")
#' @export


hist_ou_densi<-function(df,variavel,tipo){


#Critérios de parada

  if ( !(variavel %in% (colnames(df)) )) {
      stop(paste0("coluna ", variavel, " nao existem em df"))

  }
      else if( class(df[,variavel]) != "numeric" ){
        stop(paste0(variavel ," não é do tipo numeric"))

      }

#FUnções de plotagem

  densidade<-function(){
    ggplot(aes( x=.data[[variavel]]), data = df ) +geom_density()
  }
  histograma<-function(){
    ggplot(aes( x=.data[[variavel]]), data = df ) +geom_histogram()
  }


#condicional pro tipo de plotagem
  switch (tipo,
    "densidade" = densidade(),
   "histograma" = histograma(),
  stop("tipo do grafico é inválido")
  )
}
