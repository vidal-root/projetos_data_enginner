
update_value_column <- function(data_set, colunm, valor1, valor2){
  comando = paste("data_set$", colunm, "[data_set$", colunm, " == '", valor1 ,"'] <- '", valor2, "'" , sep= '')
  print(comando)
  eval(parse(text = comando))
  return(data_set)
}

update_name_column <- function(data_set, colunm, nm_column){
  comando = paste("colnames(data_set)[", colunm ,"] <- '", nm_column , "'" , sep= '')
  print(comando)
  eval(parse(text = comando))
  return(data_set)
}
