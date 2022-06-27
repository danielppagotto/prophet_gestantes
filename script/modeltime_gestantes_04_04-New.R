library(tidyverse); library(lubridate); library(modeltime); library(tidymodels)
library(timetk); library(readxl)

setwd("~/GitHub/prophet_gestantes/bases") 

municipio_regiao <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/municipio_regiao.csv", 
                             col_types = cols(CO_MUNICIP = col_character(), 
                                              CO_REGSAUD = col_character())) %>% 
                    janitor::clean_names()

nascimentos <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/nascimentos.csv") %>% 
               select(-X1) %>% janitor::clean_names()

#nascimentos <- nascimentos %>%
#  select(-...1) %>%
#  janitor::clean_names()


regioes_nomes <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/regioes_nomes.csv", 
                          col_types = cols(CO_REGSAUD = col_character())) %>% 
                 janitor::clean_names() %>% 
                 select(co_regsaud, ds_nomepad)

municipios_macrorregiao_saude <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/municipios_macrorregiao_saude.csv") 

#nascimentos$mes_ano <- zoo::as.yearmon(nascimentos$data, "%Y %m")


nascimentos <- nascimentos %>% 
                    mutate(data = substr(dtnasc, 3, 8))

nascimentos$data <- my(nascimentos$data)
                    

nascimentos_go <- nascimentos %>% 
                    filter(data < "2021-10-01") %>%
                    mutate(codmunres6 = as.numeric(substr(codmunres, 1, 6))) %>% 
                    left_join(municipios_macrorregiao_saude, by = c("codmunres6"="cod_municipio")) %>% 
                    group_by(macrorregiao, data) %>%
                    summarise(total = sum(contagem)) %>% 
                    filter(macrorregiao != "NA") %>% 
                    mutate(status = 'atual')


nascimentos_go %>% 
  ggplot(aes(x = data, y = total, col = macrorregiao)) + geom_line() + 
  theme_minimal()


datas <- nascimentos_go %>% 
                ungroup() %>% 
                select(data) %>% 
                distinct(data) 



# nascimentos_go <- nascimentos %>% 
#                       filter(data < "2021-10-01") %>%
#                       left_join(municipio_regiao, by = c("regiao"="co_municip")) %>% 
#                       left_join(regioes_nomes, by = c("co_regsaud" = "co_regsaud")) %>% 
#                       group_by(uf, data) %>%
#                       summarise(total = sum(contagem))                    
                    
nascimentos_go %>% 
  ggplot(aes(x = data, y = total)) + geom_line(size = 0.60) + 
  theme_minimal() + facet_wrap(~macrorregiao)

nascimentos_go %>% 
  plot_time_series(data, total)

# split e prophet ---------------------------------------------------------

splits <- time_series_split(
  nascimentos_go,
  assess = "12 months",
  cumulative = TRUE
)

splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(data, total)

splits

# modelos -----------------------------------------------------------------

model_arima <- arima_reg() %>% 
                set_engine("auto_arima") %>% 
                fit(total ~ data, training(splits))

model_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
                 set_engine("prophet") %>% 
                 fit(total ~ data, training(splits))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ data, data = training(splits))

model_tbl <- modeltime_table(
  model_arima,
  model_prophet,
  model_fit_ets
)

# Calibrate ---------------------------------------------------------------

calib_tbl <- model_tbl %>% 
    modeltime_calibrate(testing(splits))

calib_tbl %>% modeltime_accuracy()

prophet_treino <- calib_tbl[[5]][[2]]

prophet_treino %>% 
  ggplot(aes(x = data)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()


# test set visualization --------------------------------------------------

calib_tbl %>% 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = nascimentos_go
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl <- calib_tbl %>% 
  modeltime_refit(nascimentos_go) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go)

future_forecast_tbl %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

# previsao do numero nascimentos -----------------------------------------------

total_nascimentos_previsao <- 
  future_forecast_tbl %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previs?o")


total_nascimentos_atual_recente <- 
  future_forecast_tbl %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual")


#writexl::write_xlsx(total_nascimentos_previsao, "totalnascimentos_go.xlsx")

total_nascimentos <- rbind(total_nascimentos_atual_recente, total_nascimentos_previsao)


total_nascimentos %>% 
  ggplot(aes(mes_ano, total,  group = 1, col = tipo)) + geom_line() + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




# Previsoes por regioees ---------------------------------------------------

# macrorregiao centro-oeste -----------------------------------------------------------------

regiao_centro_oeste <- nascimentos_go %>% 
  filter(macrorregiao == "macrorregião Centro-Oeste") %>% 
  ungroup() %>% 
  select(-macrorregiao)

#----------------------------------------------------------------------------------
splits_regiao_centro_oeste <- time_series_split(
  regiao_centro_oeste,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_centro_oeste %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(data, total)

splits_regiao_centro_oeste

#----------------------------------------------------------------------------------
model_arima_regiao_centro_oeste <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ data, training(splits_regiao_centro_oeste))

model_prophet_regiao_centro_oeste <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ data, training(splits_regiao_centro_oeste))

model_fit_ets_regiao_centro_oeste <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ data, data = training(splits_regiao_centro_oeste))

model_tbl_regiao_centro_oeste <- modeltime_table(
  model_arima_regiao_centro_oeste,
  model_prophet_regiao_centro_oeste,
  model_fit_ets_regiao_centro_oeste
)

# ----------------------------------------------------------------------------------

calib_tbl_regiao_centro_oeste <- model_tbl_regiao_centro_oeste %>% 
  modeltime_calibrate(testing(splits_regiao_centro_oeste))

calib_tbl_regiao_centro_oeste %>% modeltime_accuracy()

prophet_treino_regiao_centro_oeste <- calib_tbl_regiao_centro_oeste[[5]][[3]]

prophet_treino_regiao_centro_oeste %>% 
  ggplot(aes(x = data)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

# ----------------------------------------------------------------------------------

calib_tbl_regiao_centro_oeste %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_centro_oeste),
    actual_data = regiao_centro_oeste
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_centro_oeste <- calib_tbl_regiao_centro_oeste %>% 
  modeltime_refit(regiao_centro_oeste) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = regiao_centro_oeste)

future_forecast_tbl_regiao_centro_oeste %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_centro_oeste %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

# ----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_centro_oeste <- 
  future_forecast_tbl_regiao_centro_oeste %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsto")%>% 
  mutate(regiao = "centro_oeste")



total_nascimentos_atual_recente_regiao_centro_oeste <- 
  future_forecast_tbl_regiao_centro_oeste %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "centro_oeste")



# macrorregiao Sudoeste -----------------------------------------------------------------

regiao_sudoeste <- nascimentos_go %>% 
  filter(macrorregiao == "Macrorregião Sudoeste") %>% 
  ungroup() %>% 
  select(-macrorregiao)

#----------------------------------------------------------------------------------
splits_regiao_sudoeste <- time_series_split(
  regiao_sudoeste,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_sudoeste %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(data, total)

splits_regiao_sudoeste

#----------------------------------------------------------------------------------
model_arima_regiao_sudoeste <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ data, training(splits_regiao_sudoeste))

model_prophet_regiao_sudoeste <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ data, training(splits_regiao_sudoeste))

model_fit_ets_regiao_sudoeste <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ data, data = training(splits_regiao_sudoeste))

model_tbl_regiao_sudoeste <- modeltime_table(
  model_arima_regiao_sudoeste,
  model_prophet_regiao_sudoeste,
  model_fit_ets_regiao_sudoeste
)

# ----------------------------------------------------------------------------------

calib_tbl_regiao_sudoeste <- model_tbl_regiao_sudoeste %>% 
  modeltime_calibrate(testing(splits_regiao_sudoeste))

calib_tbl_regiao_sudoeste %>% modeltime_accuracy()

prophet_treino_regiao_sudoeste <- calib_tbl_regiao_sudoeste[[5]][[3]]

prophet_treino_regiao_sudoeste %>% 
  ggplot(aes(x = data)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

# ----------------------------------------------------------------------------------

calib_tbl_regiao_sudoeste %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_sudoeste),
    actual_data = regiao_sudoeste
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_sudoeste <- calib_tbl_regiao_sudoeste %>% 
  modeltime_refit(regiao_sudoeste) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = regiao_sudoeste)

future_forecast_tbl_regiao_sudoeste %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_sudoeste %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

# ----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_sudoeste <- 
  future_forecast_tbl_regiao_sudoeste %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previs?o")%>% 
  mutate(regiao = "sudoeste")

total_nascimentos_atual_recente_regiao_sudoeste <- 
  future_forecast_tbl_regiao_sudoeste %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "sudoeste")


# macrorregiao Nordeste -----------------------------------------------------------------

regiao_nordeste <- nascimentos_go %>% 
  filter(macrorregiao == "Macrorregião Nordeste") %>% 
  ungroup() %>% 
  select(-macrorregiao)


#----------------------------------------------------------------------------------
splits_regiao_nordeste <- time_series_split(
  regiao_nordeste,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_nordeste %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(data, total)

splits_regiao_nordeste

#----------------------------------------------------------------------------------
model_arima_regiao_nordeste <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ data, training(splits_regiao_nordeste))

model_prophet_regiao_nordeste <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ data, training(splits_regiao_nordeste))

model_fit_ets_regiao_nordeste <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ data, data = training(splits_regiao_nordeste))

model_tbl_regiao_nordeste <- modeltime_table(
  model_arima_regiao_nordeste,
  model_prophet_regiao_nordeste,
  model_fit_ets_regiao_nordeste
)

# ----------------------------------------------------------------------------------

calib_tbl_regiao_nordeste <- model_tbl_regiao_nordeste %>% 
  modeltime_calibrate(testing(splits_regiao_nordeste))

calib_tbl_regiao_nordeste %>% modeltime_accuracy()

prophet_treino_regiao_nordeste <- calib_tbl_regiao_nordeste[[5]][[1]]

prophet_treino_regiao_nordeste %>% 
  ggplot(aes(x = data)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

# ----------------------------------------------------------------------------------

calib_tbl_regiao_nordeste %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_nordeste),
    actual_data = regiao_nordeste
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_nordeste <- calib_tbl_regiao_nordeste %>% 
  modeltime_refit(regiao_nordeste) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = regiao_nordeste)

future_forecast_tbl_regiao_nordeste %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_nordeste %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

# ----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_nordeste <- 
  future_forecast_tbl_regiao_nordeste %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsto")%>% 
  mutate(regiao = "nordeste")


total_nascimentos_atual_recente_regiao_nordeste <- 
  future_forecast_tbl_regiao_nordeste %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "nordeste")


# macrorregiao Centro-Norte -----------------------------------------------------------------

regiao_centro_norte <- nascimentos_go %>% 
  filter(macrorregiao == "Macrorregião Centro-Norte") %>% 
  ungroup() %>% 
  select(-macrorregiao)

#----------------------------------------------------------------------------------
splits_regiao_centro_norte <- time_series_split(
  regiao_centro_norte,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_centro_norte %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(data, total)

splits_regiao_centro_norte

#----------------------------------------------------------------------------------
model_arima_regiao_centro_norte <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ data, training(splits_regiao_centro_norte))

model_prophet_regiao_centro_norte <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ data, training(splits_regiao_centro_norte))

model_fit_ets_regiao_centro_norte <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ data, data = training(splits_regiao_centro_norte))

model_tbl_regiao_centro_norte <- modeltime_table(
  model_arima_regiao_centro_norte,
  model_prophet_regiao_centro_norte,
  model_fit_ets_regiao_centro_norte
)

# ----------------------------------------------------------------------------------

calib_tbl_regiao_centro_norte <- model_tbl_regiao_centro_norte %>% 
  modeltime_calibrate(testing(splits_regiao_centro_norte))

calib_tbl_regiao_centro_norte %>% modeltime_accuracy()

prophet_treino_regiao_centro_norte <- calib_tbl_regiao_centro_norte[[5]][[3]]

prophet_treino_regiao_centro_norte %>% 
  ggplot(aes(x = data)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

# ----------------------------------------------------------------------------------

calib_tbl_regiao_centro_norte %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_centro_norte),
    actual_data = regiao_centro_norte
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_centro_norte <- calib_tbl_regiao_centro_norte %>% 
  modeltime_refit(regiao_centro_norte) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = regiao_centro_norte)

future_forecast_tbl_regiao_centro_norte %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_centro_norte %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

# ----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_centro_norte <- 
  future_forecast_tbl_regiao_centro_norte %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsto") %>% 
  mutate(regiao = "Centro Norte")


total_nascimentos_atual_recente_regiao_centro_norte <- 
  future_forecast_tbl_regiao_centro_norte %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "Centro Norte")


# macrorregiao Centro-Sudeste -----------------------------------------------------------------

regiao_centro_sudeste <- nascimentos_go %>% 
  filter(macrorregiao == "Macrorregião Centro Sudeste") %>% 
  ungroup() %>% 
  select(-macrorregiao)

#----------------------------------------------------------------------------------
splits_regiao_centro_sudeste <- time_series_split(
  regiao_centro_sudeste,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_centro_sudeste %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(data, total)

splits_regiao_centro_sudeste

#----------------------------------------------------------------------------------
model_arima_regiao_centro_sudeste <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ data, training(splits_regiao_centro_sudeste))

model_prophet_regiao_centro_sudeste <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ data, training(splits_regiao_centro_sudeste))

model_fit_ets_regiao_centro_sudeste <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ data, data = training(splits_regiao_centro_sudeste))

model_tbl_regiao_centro_sudeste <- modeltime_table(
  model_arima_regiao_centro_sudeste,
  model_prophet_regiao_centro_sudeste,
  model_fit_ets_regiao_centro_sudeste
)

# ----------------------------------------------------------------------------------

calib_tbl_regiao_centro_sudeste <- model_tbl_regiao_centro_sudeste %>% 
  modeltime_calibrate(testing(splits_regiao_centro_sudeste))

calib_tbl_regiao_centro_sudeste %>% modeltime_accuracy()

prophet_treino_regiao_centro_sudeste <- calib_tbl_regiao_centro_sudeste[[5]][[3]]

prophet_treino_regiao_centro_sudeste %>% 
  ggplot(aes(x = data)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

# ----------------------------------------------------------------------------------

calib_tbl_regiao_centro_sudeste %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_centro_sudeste),
    actual_data = regiao_centro_sudeste
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_centro_sudeste <- calib_tbl_regiao_centro_sudeste %>% 
  modeltime_refit(regiao_centro_sudeste) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = regiao_centro_sudeste)

future_forecast_tbl_regiao_centro_sudeste %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_centro_sudeste %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

# ----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_centro_sudeste <- 
  future_forecast_tbl_regiao_centro_sudeste %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previs?o") %>% 
  mutate(regiao = "Centro Sudeste")


total_nascimentos_atual_recente_regiao_centro_sudeste <- 
  future_forecast_tbl_regiao_centro_sudeste %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "Centro Sudeste")


nascimentos_previsao <- rbind(total_nascimentos_previsao_regiao_centro_norte,
                     total_nascimentos_previsao_regiao_centro_oeste,
                     total_nascimentos_previsao_regiao_centro_sudeste,
                     total_nascimentos_previsao_regiao_nordeste,
                     total_nascimentos_previsao_regiao_sudoeste)

nascimentos_previsao$mes_ano_data <- ym(nascimentos_previsao$mes_ano)

nascimentos_previsao %>% 
  ggplot(aes(x = mes_ano_data, y = total, col = regiao)) + geom_line()

# write.csv(nascimentos_previsao, "nascimentos_previsao.csv")
# writexl::write_xlsx(nascimentos_previsao, "nascimentos_previsao.xlsx")



# Tempos ------------------------------------------------------------------


nascimentos_previsao_tempos <- read_excel("~/GitHub/prophet_gestantes/bases/nascimentos_previsao_tempos.xlsx")

nascimentos_previsao_atual <- nascimentos_previsao_tempos %>% 
                                  select(regiao, mes_ano_data, total) %>% 
                                  mutate(status = "previsto") %>% 
                                  rename(macrorregiao = regiao, 
                                         data = mes_ano_data)


nascimentos_previsao_atual <- rbind(nascimentos_previsao_atual, nascimentos_go)

nascimentos_previsao_atual <- nascimentos_previsao_atual %>% 
                                  mutate(macrorregiao = case_when(macrorregiao == "Centro Norte" | 
                                                                  macrorregiao == "Macrorregi?o Centro-Norte" ~ "Centro-Norte",
                                                                  macrorregiao == "Centro Oeste" | 
                                                                  macrorregiao == "macrorregi?o Centro-Oeste" ~ "Centro-Oeste",
                                                                  macrorregiao == "Macrorregi?o Centro Sudeste" | 
                                                                  macrorregiao == "Centro Sudeste" ~ "Centro-Sudeste",
                                                                  macrorregiao == "Macrorregi?o Nordeste" | macrorregiao == "Nordeste" ~ "Nordeste",
                                                                  macrorregiao == "Macrorregi?o Sudoeste" | macrorregiao == "Sudoeste" ~ "Sudoeste"))

nascimentos_previsao_atual %>% 
  group_by(macrorregiao) %>% 
  count()

# total de nascimentos  ---------------------------------------------------


nascimentos_previsao_atual %>% 
  ggplot(aes(x = data, y = total, col = macrorregiao)) + geom_line(size = 1) + 
  theme_minimal() + geom_vline(xintercept=as.numeric(ymd("2021-01-01")), linetype="dashed",
                               color = "blue", size=1.5) +
  theme(text = element_text(size = 22)) + xlab("Ano") +
  theme(legend.position = 'bottom')




# tempo de procedimentos  -------------------------------------------------

tempos_procedimentos <- read_excel("~/GitHub/prophet_gestantes/bases/nascimentos_previsao_tempos.xlsx", 
                                          sheet = "tempo de procedimentos")


tempos <- tempos_procedimentos %>% 
  select(-mes_ano, -ano) %>% 
  group_by(regiao) %>% 
  summarise_all(mean) %>% 
  gather(key = "Procedimento",
         value = "Tempo em horas",
         2:14) %>% 
  separate(Procedimento, c("Procedimento","Nível de atenção"), ",") %>% 
  mutate(atencao = as.numeric(`Nível de atenção`)) %>% 
  mutate(Atencao = case_when(atencao == 1 ~ "Primária",
                             atencao == 2 ~ "Especializada"))


tempos %>% 
  ggplot(aes(x = fct_reorder(Procedimento,`Tempo em horas`), 
             y = `Tempo em horas`, fill = Atencao)) + 
  geom_col() + coord_flip() + theme_minimal() + 
  xlab("Procedimento") + ggtitle("Tempo m?dio por procedimento", 
                                 "M?dia de tempos dos tr?s anos projetados") + 
  facet_grid(~regiao) + theme(text = element_text(size = 25)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# tempos de profissionais -------------------------------------------------


tempos_procedimentos <- nascimentos_previsao_tempos %>% 
  select(ano, regiao, starts_with("tempo_"), pessoal40h, medico_ab,
         medico_esp,	enf_ab) %>% 
  group_by(ano, regiao) %>% 
  summarise_all(sum) 


# tempo_profissionais <- tempos_procedimentos %>% 
#                           mutate(tempo_medico_ab = (0.5 * tempo_consulta_prenatal +
#                                                     0.5 * tempo_consulta_puerperal +
#                                                     0.5 * tempo_acoes_educativas + 
#                                                     tempo_ultrassonografia_obstetrica +
#                                                     tempo_exame_citopatologico)/960, 
#                                 tempo_medico_esp = (tempo_consulta_especializada +
#                                                     tempo_ecg + 
#                                                     tempo_ultrassom_obstetrico_dopler + 
#                                                     tempo_ultrassom_obstetrico + 
#                                                     tempo_tococardiografia + 
#                                                     tempo_parto_cesarea + 
#                                                     tempo_parto_normal_sem_instrumenta??o)/960,
#                                 tempo_enfermeiro = (0.5 * tempo_consulta_prenatal +
#                                                     0.5 * tempo_consulta_puerperal +
#                                                     0.5 * tempo_acoes_educativas)/960) %>% 
#                           select(batch, ano, regiao, tempo_medico_ab, tempo_medico_esp, 
#                                  tempo_enfermeiro) %>% 
#                           gather(key = "Profissional",
#                                  value = "total",
#                                  4:6) %>% ungroup() %>% select(-ano)




demanda_profissionais <- tempos_procedimentos %>% 
    select(ano, regiao, medico_ab, medico_esp, enf_ab) %>% 
  gather(key = "Profissional",
         value = "total",
         3:5) %>% ungroup() %>% 
  mutate(cenario = "demanda")



# 
# tempo_profissionais$batch <- as.factor(tempo_profissionais$batch) 
# tempo_profissionais$batch <- ordered(tempo_profissionais$batch, 
#                                     levels = c('1/22', '2/22',
#                                                '1/23', '2/23',
#                                                '1/24', '2/24'))
#                                 
# 
# #writexl::write_xlsx(tempo_profissionais, "tempo_profissionais.xlsx")
# 
# tempo_profissionais <- tempo_profissionais %>% 
#                             mutate(id = case_when(batch == '1/22' ~ 1, 
#                                                    batch == '2/22' ~ 2,
#                                                    batch == '1/23' ~ 3,
#                                                    batch == '2/23' ~ 4,
#                                                    batch == '1/24' ~ 5,
#                                                    batch == '2/24' ~ 6))
                                               

demanda_profissionais %>% 
  ggplot(aes(x = ano, y = total, 
             fill = Profissional, group = 1)) + 
  geom_col(group = 1, position = "dodge") + facet_grid(~regiao) + 
  theme_minimal() + theme(text = element_text(size = 22)) + xlab("Ano") + 
  ylab("Total de profissionais em ETI40") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_discrete(labels = c("Enfermeiro AB", "M?dico AB", "M?dico AS")) + 
  theme(legend.position = 'bottom')



# Oferta de profissionais -------------------------------------------------

oferta_consolidada <- read_excel("~/GitHub/prophet_gestantes/bases/oferta consolidada.xlsx")

oferta_consolidada <- oferta_consolidada %>%  
  mutate(ano = year(mes_ano)) %>% 
  filter(ano > 2021) %>% 
  group_by(macrorregiao, ano, nivel_atencao, 
           categoria, cen?rio) %>% 
  summarise(total = sum(qtd_mensal)) %>% 
  ungroup() %>% 
  select(ano, macrorregiao, categoria, total, cen?rio) %>% 
  rename(regiao = macrorregiao, Profissional = categoria)


# comparando oferta e demanda ---------------------------------------------

demanda_oferta <- rbind(oferta_consolidada, demanda_profissionais)


demanda_oferta %>% 
  filter(Profissional == "enf_ab") %>% 
  ggplot(aes(x = ano, y = total, col = cen?rio)) + geom_line(size = 1) + geom_point() +
  geom_label(aes(label = round(total), fill = cen?rio), colour = "white", fontface = "bold", size = 8) + 
  theme_minimal() + facet_wrap(~regiao, nrow = 2, ncol = 3) + scale_x_continuous(breaks = seq(2022, 2024, 1)) +
  theme(legend.position = 'bottom') + theme(text = element_text(size = 23)) +
  guides(col=guide_legend(nrow=2,byrow=TRUE)) + 
  ggtitle("Demanda vs Oferta - Enfermeiro (AB)")



demanda_oferta %>% 
  filter(Profissional == "medico_ab") %>% 
  ggplot(aes(x = ano, y = total, col = cen?rio)) + geom_line(size = 1) + geom_point() +
  geom_label(aes(label = round(total), fill = cen?rio), colour = "white", fontface = "bold", size = 8) + 
  theme_minimal() + facet_wrap(~regiao, nrow = 2, ncol = 3) + scale_x_continuous(breaks = seq(2022, 2024, 1)) +
  theme(legend.position = 'bottom') + theme(text = element_text(size = 23)) +
  guides(col=guide_legend(nrow=2,byrow=TRUE)) +
  ggtitle("Demanda vs Oferta - M?dico (AB)")



demanda_oferta %>% 
  filter(Profissional == "medico_esp") %>% 
  ggplot(aes(x = ano, y = total, col = cen?rio)) + geom_line(size = 1) + geom_point() +
  geom_label(aes(label = round(total), fill = cen?rio), colour = "white", fontface = "bold", size = 8) + 
  theme_minimal() + facet_wrap(~regiao, nrow = 2, ncol = 3) + scale_x_continuous(breaks = seq(2022, 2024, 1)) +
  theme(legend.position = 'bottom') + theme(text = element_text(size = 23)) +
  guides(col=guide_legend(nrow=2,byrow=TRUE)) + 
  ggtitle("Demanda vs Oferta - M?dico (AS)")




# calculando deficits -----------------------------------------------------

deficit <- demanda_oferta %>% 
  mutate(status = case_when(cen?rio == "Oferta - Cen?rio 1 - constante" ~ "oferta1", 
                            cen?rio == "Oferta - Cen?rio 2 - aumento de 5% ao ano" ~ "oferta2",
                            cen?rio == "Oferta - Cen?rio 3 - altera??o para os n?veis pr?-pandemia (12/2019)" ~ "oferta3",
                            cen?rio == "demanda" ~ "demanda"
                            )) %>% 
  select(-cen?rio) %>% 
  spread(status, total) %>% 
  group_by(regiao, Profissional) %>% 
  summarize_all(mean) %>% 
  mutate(resultado1 = oferta1 - demanda,
         resultado2 = oferta2 - demanda,
         resultado3 = oferta3 - demanda) 


writexl::write_xlsx(deficit, "deficit.xlsx")


deficit_gather <- deficit %>%  
                  select(ano, regiao, Profissional, 
                         resultado1, resultado2, resultado3) %>% 
                  gather(key = cenario, value = "resultado", 
                         4:6)
  

deficit_gather %>% 
  ggplot(aes(x = ano, y = resultado, fill = regiao)) + geom_col(position = "dodge") + 
  facet_grid(~Profissional) + coord_flip() + theme_minimal()
