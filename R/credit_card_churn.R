# Carregando pacotes ------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(magrittr)
library(janitor)
library(skimr)
library(funModeling)
library(vip)
library(naniar)


# Carregando a base de dados ----------------------------------------------

data_raw <- read_csv("dados/BankChurners.csv")

# Visualizando as primeiras observações
data_raw %>% head(10) %>% View()

# Temos 10127 registros e 23 variáveis.
# Porém, as duas últimas colunas serão desconsideradas, conforme
# orientado na descrição do arquivo.

# Ajustando o nome das colunas e removendo as duas últimas colunas
data <- data_raw %>%
  clean_names() %>%
  select_at(vars(-22, -23))


# 1 - Treino, Teste e Validação -------------------------------------------

set.seed(202107)

churn_initial_split <- initial_split(data, strata = attrition_flag, prop = 0.75)

data_train <- training(churn_initial_split)
data_test <- testing(churn_initial_split)


# 2 - EDA -----------------------------------------------------------------

# formato das variáveis
glimpse(data)
skim(data)
data %>% status # quantidade de zeros

# Dados faltantes
vis_miss(data)

# Proporção da variável resposta
freq(data %>% select(attrition_flag))

# Visualizando as variáveis numéricas
plot_num(data %>% select(-clientnum))

# Visualizando as variáveis categóricas
freq(
  data %>%
    select(-attrition_flag) %>%
    select_if(is.character)
  )

# Variável target vs numéricas
plotar(
  data,
  input = c("customer_age", "dependent_count", "months_on_book",
            "total_relationship_count", "months_inactive_12_mon", "contacts_count_12_mon", "credit_limit",
            "total_revolving_bal", "avg_open_to_buy", "total_amt_chng_q4_q1", "total_trans_amt",
            "total_trans_ct", "total_ct_chng_q4_q1", "avg_utilization_ratio"),
  target = "attrition_flag",
  plot_type = "boxplot"
)

# Variável target vs categóricas
cross_plot(
  data,
  input = c("gender", "education_level", "marital_status",
            "income_category", "card_category"),
  target = "attrition_flag"
)

# Correlação entre as variáveis numéricas
data %>%
  select(-clientnum) %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot(
    method = "color",
    addCoef.col = TRUE,
    number.cex = 0.7,
    diag = FALSE,
    type = "lower",
    tl.srt = 25,
    tl.col = "black"
  )





