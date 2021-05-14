
#DataSet = https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients#
#X1: Valor do crédito concedido (dólar NT): inclui tanto o crédito ao consumidor pessoa física quanto o crédito familiar (suplementar).
#SEX: Gênero (1 = masculino; 2 = feminino).
#EDUCATION: Educação (1 = pós-graduação; 2 = universidade; 3 = ensino médio; 4 = outros).
#MARRIAGE: Estado civil (1 = casado; 2 = solteiro; 3 = outros).
#X5: Idade (ano).
#X6 - 
#X11: Histórico de pagamentos anteriores. Rastreamos os registros de pagamentos mensais anteriores (de abril a setembro de 2005) da seguinte maneira: X6 = o status de reembolso em setembro de 2005; X7 = situação de amortização em agosto de 2005; . . .; X11 = estado de reembolso em abril de 2005. A escala de medição para o estado de reembolso é: -1 = pagamento em dia; 1 = atraso no pagamento por um mês; 2 = atraso no pagamento por dois meses; . . .; 8 = atraso no pagamento por oito meses; 9 = atraso no pagamento de nove meses ou mais.
#X12-X17: Valor da fatura (dólar NT). X12 = valor da fatura em setembro de 2005; X13 = valor da fatura em agosto de 2005; . . .; X17 = valor da fatura em abril de 2005.
#X18-SEEDUCATION: Valor do pagamento anterior (dólar NT). X18 = valor pago em setembro de 2005; X19 = valor pago em agosto de 2005; . . .; SEEDUCATION = valor pago em abril de 2005.

#setar diretorio
setwd("C:/Users/vidal_root/Documents/Estudo/MBA - Data Enginner/Coleta e Preparação dos Dados/Atividades/projeto_atividade_previa")

#carregar arquivo de functions
source(file = 'functions.R')

#carregar dados
data_set_cred_clients <- read.table("default of credit card clients.csv", header = TRUE, skip = 1 , sep = ",", dec = ".")
View(data_set_cred_clients)

#Questão 1
# --- A ---
str(data_set_cred_clients)
# --- B ---
dim(data_set_cred_clients)
# --- C --- 
summary(data_set_cred_clients)

# --------------------------------------- Tratamento dos dados --------------------------------------- 
# Update Registro

data_set_cred_clients <- update_value_column(data_set_cred_clients, 'SEX', "1", "Masculino")
data_set_cred_clients <- update_value_column(data_set_cred_clients, 'SEX', "2", "Feminino")
data_set_cred_clients <- update_value_column(data_set_cred_clients, 'EDUCATION', "1", "Pós-graduação")
data_set_cred_clients <- update_value_column(data_set_cred_clients, 'EDUCATION', "2", "Universidade")
data_set_cred_clients <- update_value_column(data_set_cred_clients, 'EDUCATION', "3", "Ensino médio")
data_set_cred_clients <- update_value_column(data_set_cred_clients, 'EDUCATION', "4", "Outros")
data_set_cred_clients <- update_value_column(data_set_cred_clients, 'MARRIAGE', "1", "Casado")
data_set_cred_clients <- update_value_column(data_set_cred_clients, 'MARRIAGE', "2", "Solteiro")
data_set_cred_clients <- update_value_column(data_set_cred_clients, 'MARRIAGE', "3", "Outros")

total_subistituir <- c('Sem informação','Pagamento em dia', 'Sem informação', 'Atraso no pagamento por um mês', 'Atraso no pagamento por dois', 'Atraso no pagamento por três meses',
                       'Atraso no pagamento por quatro meses', 'Atraso no pagamento por cinco meses', 'Atraso no pagamento por seis meses', 
                       'Atraso no pagamento por sete meses', 'Atraso no pagamento por oito meses', 'Atraso no pagamento por nove meses ou mais')

i = -2;

for (value in total_subistituir) {
  
  total_tables <- c(0:6)
  
  for (table in total_tables) {
    nm_table <- paste('PAY_', table, sep = '')
    if(nm_table != 'PAY_1'){
      data_set_cred_clients <- update_value_column(data_set_cred_clients, nm_table, i, value)
    }
  }
  
  i = i + 1
}

View(data_set_cred_clients)

# Column rename
nm_columns = c('Crédito concedido', 'Gênero', 'Educação', 'Estado civil', 'Idade' ,'Reembolso em setembro','Amortização em agosto','Amortização em julho','Amortização em junho','Amortização em agosto'
               ,'Reembolso em abril','Valor da fatura em setembro','Valor da fatura em agosto','Valor da fatura em julho'
               ,'Valor da fatura em junho','Valor da fatura em maio','Valor da fatura em abril','Valor pago em setembro'
               ,'Valor pago em agosto','Valor pago em julho','Valor pago em junho','Valor pago em maio','Valor pago em abril')

idx_column = 2

for (column in nm_columns) {
  data_set_cred_clients <- update_name_column(data_set_cred_clients, idx_column, column)
  idx_column = idx_column + 1
}

View(data_set_cred_clients)


