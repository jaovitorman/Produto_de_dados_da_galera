library(shiny)
library(dplyr)
library(purrr)
library(ggplot2)

Dados <-  read.csv("games.csv")

Dados_n <- Dados |> dplyr::select("Platform", "Genre", "Rating", "NA_Sales", "EU_Sales",
                                  "JP_Sales", "Other_Sales", "Global_Sales")



#variaveis de teste

Dados_n$Global_Sales <- Dados_n$Global_Sales*100



#Função que faz ic para uma amostra
ic_medias <- function(x,alpha){
  teste_poisson<-function(y,categoria){
    print(class(y))
    print(sum(y))
    resultado_teste<-poisson.test(x= sum(y) ,T= length(y),r = mean(y) ,conf.level =  alpha)
    retorno<-list(a=resultado_teste$conf.int[1],b=resultado_teste$conf.int[2],c=mean(y),d=categoria)
    return(retorno)
  }
  resultados<- map(unique(x$categoria), function(y) teste_poisson(x[x$categoria==y,"vendas"], y))
  resultados_lista <- map(resultados,\(x) as.data.frame(x))
  resultados_df <- list_rbind(resultados_lista)
  return(resultados_df)
}


ui <- fluidPage(
  titlePanel("Vendas de Jogos"),
  
  selectInput("categoria", "Selecione uma categoria:", c("Platform", "Genre", "Rating")),
  numericInput("alpha", "Qual o nivél de significancia:", 0.95, min = 0.1, max = 0.99),
  plotOutput("Grafico_1")
)

server <- function(input, output) {
  
  output$Grafico_1 <- renderPlot({
    df <- Dados_n |> select("Global_Sales", input$categoria)
    df <- df |> na.omit()
    names(df) <- c("vendas","categoria")
    #Lembrete deixar o gráfico mais bonito no momento está horrorso
    df_ic <- ic_medias(df, input$alpha)
    names(df_ic)<-c("intervalo inferior","intervalo superior", "medias", "categoria" )
    
    #ggplot(aes(y=medias , x =categoria ) ,data=teste2) +geom_point()+ geom_point(aes(y=`intervalo inferior`),color="pink") +
    #geom_point(aes(y=`intervalo superior`), color="green") + theme_minimal()
    ggplot(aes(y=medias , x =categoria ) ,data=df_ic) +geom_point()+ geom_errorbar(aes(ymax=`intervalo superior`, ymin=`intervalo inferior`))
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
########################################################################################################################################################################################
####################################################      Função que coloca intervalo                                    ###############################################################
########################################################################################################################################################################################
########################################################################################################################################################################################


#' Intervalo de confiança para vendas
#'
#' @description ´
#' Função recebe um dataframe com vendas por categorias e retorna o intervalo de confiança, para a média
#' de vendas para cada uma das categorias.

#' @param x  um data frame com duas colunas uma chamda " vendas que contém o número de vendas e outra "categoria"
#' que contem uma string com o nome da categoria
#' @param alpha Nível de significancia do intervalo de confiança.
#'
#' @returns Uma data frame contendo , intervalo superior e inferior do ic de vendas, e a média para cada categoria.
#'
#' @import dplyr, purrr
#' @export ´

#Função que faz ic para uma amostra
ic_medias <- function(x,alpha){
  teste_poisson<-function(y,categoria){
    print(class(y))
    print(sum(y))
    resultado_teste<-poisson.test(x= sum(y) ,T= length(y),r = mean(y) ,conf.level =  alpha)
    retorno<-list(a=resultado_teste$conf.int[1],b=resultado_teste$conf.int[2],c=mean(y),d=categoria)
    return(retorno)
  }
  resultados<- map(unique(x$categoria), function(y) teste_poisson(x[x$categoria==y,"vendas"], y))
  resultados_lista <- map(resultados,\(x) as.data.frame(x))
  resultados_df <- list_rbind(resultados_lista)
  names(resultados)<-c("intervalo inferior","intervalo superior", "medias", "categoria" )
  return(resultados_df)
}


########################################################################################################################################################################################
####################################################      Função que plota séries temporais                               ###############################################################
########################################################################################################################################################################################
########################################################################################################################################################################################



