
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

get_nr_dolar <- function(){
    base_url <- "https://economia.awesomeapi.com.br"
    path <- "json/last/USD-BRL"
    
    endpoint <- paste(base_url, path, sep="/")
    
    res_api <- GET(
      url = endpoint
    )
    
    res_api_text <- rawToChar(res_api$content)
    
    res_api_text_json <- fromJSON(res_api_text, flatten=TRUE)
    
    return(res_api_text_json$USDBRL$high)
    
}