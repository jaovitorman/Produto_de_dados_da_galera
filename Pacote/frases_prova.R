#'
#'
#' @export

frases_prova<-function(valor_logico){
  afirmacoes <- c(
  "Documentações de funções não podem ser criadas sem o pacote roxygen2.",
  "Funções sempre precisam retornar algum objeto.",
  "Podemos salvar objetos do R, como vetores numéricos e funções, no formato YAML.",
  "A função 'lm', utilizada para ajuste de regressão linear, não faz parte de nenhum pacote.",
  "Não existe diferença entre os operadores '=' e '<-' para atribuição de variáveis."
)

if (valor_logico == T) {
  retorno <- afirmacoes[1:4]
}else{
  retorno <- afirmacoes[5]
}
  return(retorno)
}
