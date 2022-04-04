library(tidyverse); library(lubridate); library(modeltime); library(tidymodels)
library(timetk)

setwd("~/GitHub/prophet_gestantes/bases") 

municipio_regiao <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/municipio_regiao.csv", 
                             col_types = cols(CO_MUNICIP = col_character(), 
                                              CO_REGSAUD = col_character())) %>% 
                    janitor::clean_names()

nascimentos <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/nascimentos.csv") %>% 
               select(-X1) %>% janitor::clean_names()

nascimentos$dtnasc <- dmy(nascimentos$dtnasc)

regioes_nomes <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/regioes_nomes.csv", 
                          col_types = cols(CO_REGSAUD = col_character())) %>% 
                 janitor::clean_names() %>% 
                 select(co_regsaud, ds_nomepad)


municipios_macrorregiao_saude <- read_csv("municipios_macrorregiao_saude.csv")

# nascimentos$mes_ano <- zoo::as.yearmon(nascimentos$dtnasc, "%Y %m")
nascimentos$mes_ano <- as.Date(format(nascimentos$dtnasc, "%Y-%m"))

nascimentos <- nascimentos %>% 
                  mutate(regiao = as.integer(str_sub(codmunres, end = 6)))

nascimentos_go <- nascimentos %>% 
                    filter(dtnasc < "2021-10-01") %>%
                    left_join(municipios_macrorregiao_saude, by = c("regiao"="cod_municipio")) %>% 
                    group_by(regiao, macrorregiao, dtnasc) %>%
                    summarise(total = sum(contagem)) %>% 
                    filter(macrorregiao != "NA")


datas <- nascimentos_go %>% 
                ungroup() %>% 
                select(dtnasc) %>% 
                distinct(dtnasc) %>% 
                mutate(teste = "teste")



# nascimentos_go <- nascimentos %>% 
#                       filter(dtnasc < "2021-10-01") %>%
#                       left_join(municipio_regiao, by = c("regiao"="co_municip")) %>% 
#                       left_join(regioes_nomes, by = c("co_regsaud" = "co_regsaud")) %>% 
#                       group_by(uf, dtnasc) %>%
#                       summarise(total = sum(contagem))                    
                    
nascimentos_go %>% 
  ggplot(aes(x = dtnasc, y = total)) + geom_line(size = 0.60) + 
  theme_minimal() + facet_wrap(~ds_nomepad)

nascimentos_go %>% 
  plot_time_series(dtnasc, total)

# split e prophet ---------------------------------------------------------

splits <- time_series_split(
  nascimentos_go,
  assess = "12 months",
  cumulative = TRUE
)

splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits

# modelos -----------------------------------------------------------------

model_arima <- arima_reg() %>% 
                set_engine("auto_arima") %>% 
                fit(total ~ dtnasc, training(splits))

model_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
                 set_engine("prophet") %>% 
                 fit(total ~ dtnasc, training(splits))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits))

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
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
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
  mutate(tipo = "previsão")


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




# Previsões por regiões ---------------------------------------------------

# regiao central -----------------------------------------------------------------

nascimentos_go_regiao_central <- nascimentos_go %>% 
  filter(ds_nomepad == "CENTRAL") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)

central <- datas %>% 
  left_join(nascimentos_go_regiao_central, by = "dtnasc") 

central$total[is.na(central$total)] <- 0


#----------------------------------------------------------------------------------
splits_regiao_central <- time_series_split(
  central,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_central %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_central

#----------------------------------------------------------------------------------
model_arima_regiao_central <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_central))

model_prophet_regiao_central <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_central))

model_fit_ets_regiao_central <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_central))

model_tbl_regiao_central <- modeltime_table(
  model_arima_regiao_central,
  model_prophet_regiao_central,
  model_fit_ets_regiao_central
)

# ----------------------------------------------------------------------------------

calib_tbl_regiao_central <- model_tbl_regiao_central %>% 
  modeltime_calibrate(testing(splits_regiao_central))

calib_tbl_regiao_central %>% modeltime_accuracy()

prophet_treino_regiao_central <- calib_tbl_regiao_central[[5]][[2]]

prophet_treino_regiao_central %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

# ----------------------------------------------------------------------------------

calib_tbl_regiao_central %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_central),
    actual_data = nascimentos_go_regiao_central
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_central <- calib_tbl_regiao_central %>% 
  modeltime_refit(nascimentos_go_regiao_central) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_central)

future_forecast_tbl_regiao_central %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_central %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

# ----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_central <- 
  future_forecast_tbl_regiao_central %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_central <- 
  future_forecast_tbl_regiao_central %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "central")

# regiao centro sul -----------------------------------------------------------------



nascimentos_go_regiao_centro_sul <- nascimentos_go %>% 
  filter(ds_nomepad == "CENTRO SUL")  %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)

cs <- datas %>% 
  left_join(nascimentos_go_regiao_centro_sul, by = "dtnasc")


#----------------------------------------------------------------------------------
splits_regiao_centro_sul <- time_series_split(
  nascimentos_go_regiao_centro_sul,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_centro_sul %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_centro_sul

#----------------------------------------------------------------------------------
model_arima_regiao_centro_sul <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_centro_sul))

model_prophet_regiao_centro_sul <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_centro_sul))

model_fit_ets_regiao_centro_sul <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_centro_sul))

model_tbl_regiao_centro_sul <- modeltime_table(
  model_arima_regiao_centro_sul,
  model_prophet_regiao_centro_sul,
  model_fit_ets_regiao_centro_sul
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_centro_sul <- model_tbl_regiao_centro_sul %>% 
  modeltime_calibrate(testing(splits_regiao_centro_sul))

calib_tbl_regiao_centro_sul %>% modeltime_accuracy()

prophet_treino_regiao_centro_sul <- calib_tbl_regiao_centro_sul[[5]][[2]]

prophet_treino_regiao_centro_sul %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_centro_sul %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_centro_sul),
    actual_data = nascimentos_go_regiao_centro_sul
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_centro_sul <- calib_tbl_regiao_centro_sul %>% 
  modeltime_refit(nascimentos_go_regiao_centro_sul) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_centro_sul)

future_forecast_tbl_regiao_centro_sul %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)

future_forecast_tbl_regiao_centro_sul %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_centro_sul <- 
  future_forecast_tbl_regiao_centro_sul %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_centro_sul <- 
  future_forecast_tbl_regiao_centro_sul %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "centro sul")

# regiao entorno norte -----------------------------------------------------------------

nascimentos_go_regiao_entorno_norte <- nascimentos_go %>% 
  filter(ds_nomepad == "ENTORNO NORTE") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_entorno_norte <- time_series_split(
  nascimentos_go_regiao_entorno_norte,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_entorno_norte %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_entorno_norte

#----------------------------------------------------------------------------------
model_arima_regiao_entorno_norte <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_entorno_norte))

model_prophet_regiao_entorno_norte <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_entorno_norte))

model_fit_ets_regiao_entorno_norte <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_entorno_norte))

model_tbl_regiao_entorno_norte <- modeltime_table(
  model_arima_regiao_entorno_norte,
  model_prophet_regiao_entorno_norte,
  model_fit_ets_regiao_entorno_norte
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_entorno_norte <- model_tbl_regiao_entorno_norte %>% 
  modeltime_calibrate(testing(splits_regiao_entorno_norte))

calib_tbl_regiao_entorno_norte %>% modeltime_accuracy()

prophet_treino_regiao_entorno_norte <- calib_tbl_regiao_entorno_norte[[5]][[2]]

prophet_treino_regiao_entorno_norte %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_entorno_norte %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_entorno_norte),
    actual_data = nascimentos_go_regiao_entorno_norte
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_entorno_norte <- calib_tbl_regiao_entorno_norte %>% 
  modeltime_refit(nascimentos_go_regiao_entorno_norte) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_entorno_norte)

future_forecast_tbl_regiao_entorno_norte %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl_regiao_entorno_norte %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_entorno_norte <- 
  future_forecast_tbl_regiao_entorno_norte %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_entorno_norte <- 
  future_forecast_tbl_regiao_entorno_norte %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual")  %>% 
  mutate(regiao = "entorno norte")


# regiao entorno sul -----------------------------------------------------------------

nascimentos_go_regiao_entorno_sul <- nascimentos_go %>% 
  filter(ds_nomepad == "ENTORNO SUL") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_entorno_sul <- time_series_split(
  nascimentos_go_regiao_entorno_sul,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_entorno_sul %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_entorno_sul

#----------------------------------------------------------------------------------
model_arima_regiao_entorno_sul <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_entorno_sul))

model_prophet_regiao_entorno_sul <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_entorno_sul))

model_fit_ets_regiao_entorno_sul <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_entorno_sul))

model_tbl_regiao_entorno_sul <- modeltime_table(
  model_arima_regiao_entorno_sul,
  model_prophet_regiao_entorno_sul,
  model_fit_ets_regiao_entorno_sul
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_entorno_sul <- model_tbl_regiao_entorno_sul %>% 
  modeltime_calibrate(testing(splits_regiao_entorno_sul))

calib_tbl_regiao_entorno_sul %>% modeltime_accuracy()

prophet_treino_regiao_entorno_sul <- calib_tbl_regiao_entorno_sul[[5]][[2]]

prophet_treino_regiao_entorno_sul %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_entorno_sul %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_entorno_sul),
    actual_data = nascimentos_go_regiao_entorno_sul
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_entorno_sul <- calib_tbl_regiao_entorno_sul %>% 
  modeltime_refit(nascimentos_go_regiao_entorno_sul) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_entorno_sul)

future_forecast_tbl_regiao_entorno_sul %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl_regiao_entorno_sul %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_entorno_sul <- 
  future_forecast_tbl_regiao_entorno_sul %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_entorno_sul <- 
  future_forecast_tbl_regiao_entorno_sul %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "entorno sul")


# regiao estrada de ferro -----------------------------------------------------------------

nascimentos_go_regiao_estrada_de_ferro <- nascimentos_go %>% 
  filter(ds_nomepad == "ESTRADA DE FERRO") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_estrada_de_ferro <- time_series_split(
  nascimentos_go_regiao_estrada_de_ferro,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_estrada_de_ferro %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_estrada_de_ferro

#----------------------------------------------------------------------------------
model_arima_regiao_estrada_de_ferro <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_estrada_de_ferro))

model_prophet_regiao_estrada_de_ferro <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_estrada_de_ferro))

model_fit_ets_regiao_estrada_de_ferro <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_estrada_de_ferro))

model_tbl_regiao_estrada_de_ferro <- modeltime_table(
  model_arima_regiao_estrada_de_ferro,
  model_prophet_regiao_estrada_de_ferro,
  model_fit_ets_regiao_estrada_de_ferro
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_estrada_de_ferro <- model_tbl_regiao_estrada_de_ferro %>% 
  modeltime_calibrate(testing(splits_regiao_estrada_de_ferro))

calib_tbl_regiao_estrada_de_ferro %>% modeltime_accuracy()

# Esse é ETS
prophet_treino_regiao_estrada_de_ferro <- calib_tbl_regiao_estrada_de_ferro[[5]][[3]]

prophet_treino_regiao_estrada_de_ferro %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_estrada_de_ferro %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_estrada_de_ferro),
    actual_data = nascimentos_go_regiao_estrada_de_ferro
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_estrada_de_ferro <- calib_tbl_regiao_estrada_de_ferro %>% 
  modeltime_refit(nascimentos_go_regiao_estrada_de_ferro) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_estrada_de_ferro)

future_forecast_tbl_regiao_estrada_de_ferro %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl_regiao_estrada_de_ferro %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_estrada_de_ferro <- 
  future_forecast_tbl_regiao_estrada_de_ferro %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_estrada_de_ferro <- 
  future_forecast_tbl_regiao_estrada_de_ferro %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "estrada de ferro")




# regiao nordeste I -----------------------------------------------------------------

nascimentos_go_regiao_nordeste_I <- nascimentos_go %>% 
  filter(ds_nomepad == "NORDESTE I") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_regiao_nordeste_I <- time_series_split(
  nascimentos_go_regiao_nordeste_I,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_regiao_nordeste_I %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_regiao_nordeste_I

#----------------------------------------------------------------------------------
model_arima_regiao_nordeste_I <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_regiao_nordeste_I))

model_prophet_regiao_nordeste_I <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_regiao_nordeste_I))

model_fit_ets_regiao_nordeste_I <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_regiao_nordeste_I))

model_tbl_regiao_nordeste_I <- modeltime_table(
  model_arima_regiao_nordeste_I,
  model_prophet_regiao_nordeste_I,
  model_fit_ets_regiao_nordeste_I
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_nordeste_I <- model_tbl_regiao_nordeste_I %>% 
  modeltime_calibrate(testing(splits_regiao_regiao_nordeste_I))

calib_tbl_regiao_nordeste_I %>% modeltime_accuracy()

prophet_treino_regiao_nordeste_I <- calib_tbl_regiao_nordeste_I[[5]][[2]]

prophet_treino_regiao_nordeste_I %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_nordeste_I %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_regiao_nordeste_I),
    actual_data = nascimentos_go_regiao_nordeste_I
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_nordeste_I <- calib_tbl_regiao_nordeste_I %>% 
  modeltime_refit(nascimentos_go_regiao_nordeste_I) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_nordeste_I)

future_forecast_tbl_regiao_nordeste_I %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl_regiao_nordeste_I %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_nordeste_I <- 
  future_forecast_tbl_regiao_nordeste_I %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_nordeste_I <- 
  future_forecast_tbl_regiao_nordeste_I %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "nordeste I")

# regiao nordeste II -----------------------------------------------------------------

nascimentos_go_regiao_nordeste_II <- nascimentos_go %>% 
  filter(ds_nomepad == "NORDESTE II") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_regiao_nordeste_II <- time_series_split(
  nascimentos_go_regiao_nordeste_II,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_nordeste_II %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_nordeste_II

#----------------------------------------------------------------------------------
model_arima_regiao_nordeste_II <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_nordeste_II))

model_prophet_regiao_nordeste_II <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_nordeste_II))

model_fit_ets_regiao_nordeste_II <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_nordeste_II))

model_tbl_regiao_nordeste_II <- modeltime_table(
  model_arima_regiao_nordeste_II,
  model_prophet_regiao_nordeste_II,
  model_fit_ets_regiao_nordeste_II
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_nordeste_II <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_nordeste_II))

calib_tbl_regiao_nordeste_II %>% modeltime_accuracy()

prophet_treino_regiao_nordeste_II <- calib_tbl_regiao_nordeste_II[[5]][[2]]

prophet_treino_regiao_nordeste_II %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_nordeste_II %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_nordeste_II),
    actual_data = nascimentos_go_regiao_nordeste_II
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_nordeste_II <- calib_tbl_regiao_nordeste_II %>% 
  modeltime_refit(nascimentos_go_regiao_nordeste_II) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_nordeste_II)

future_forecast_tbl_regiao_nordeste_II %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl_regiao_nordeste_II %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_nordeste_II <- 
  future_forecast_tbl_regiao_nordeste_II %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_nordeste_II <- 
  future_forecast_tbl_regiao_nordeste_II %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "nordeste II")




# regiao norte -----------------------------------------------------------------

nascimentos_go_regiao_norte <- nascimentos_go %>% 
  filter(ds_nomepad == "NORTE") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_regiao_norte <- time_series_split(
  nascimentos_go_regiao_norte,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_norte %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_norte

#----------------------------------------------------------------------------------
model_arima_regiao_norte <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_norte))

model_prophet_regiao_norte <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_norte))

model_fit_ets_regiao_norte <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_norte))

model_tbl_regiao_norte <- modeltime_table(
  model_arima_regiao_norte,
  model_prophet_regiao_norte,
  model_fit_ets_regiao_norte
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_norte <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_norte))

calib_tbl_regiao_norte %>% modeltime_accuracy()

prophet_treino_regiao_norte <- calib_tbl_regiao_norte[[5]][[2]]

prophet_treino_regiao_norte %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_norte %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_norte),
    actual_data = nascimentos_go_regiao_norte
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_norte <- calib_tbl_regiao_norte %>% 
  modeltime_refit(nascimentos_go_regiao_norte) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_norte)

future_forecast_tbl_regiao_norte %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl_regiao_norte %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_norte <- 
  future_forecast_tbl_regiao_norte %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_norte <- 
  future_forecast_tbl_regiao_norte %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "norte")




# regiao oeste I -----------------------------------------------------------------

nascimentos_go_regiao_oeste_I <- nascimentos_go %>% 
  filter(ds_nomepad == "OESTE I") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_regiao_oeste_I <- time_series_split(
  nascimentos_go_regiao_oeste_I,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_oeste_I %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_oeste_I

#----------------------------------------------------------------------------------
model_arima_regiao_oeste_I <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_oeste_I))

model_prophet_regiao_oeste_I <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_oeste_I))

model_fit_ets_regiao_oeste_I <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_oeste_I))

model_tbl_regiao_oeste_I <- modeltime_table(
  model_arima_regiao_oeste_I,
  model_prophet_regiao_oeste_I,
  model_fit_ets_regiao_oeste_I
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_oeste_I <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_oeste_I))

calib_tbl_regiao_oeste_I %>% modeltime_accuracy()

prophet_treino_regiao_oeste_I <- calib_tbl_regiao_oeste_I[[5]][[2]]

prophet_treino_regiao_oeste_I %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_oeste_I %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_oeste_I),
    actual_data = nascimentos_go_regiao_oeste_I
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_oeste_I <- calib_tbl_regiao_oeste_I %>% 
  modeltime_refit(nascimentos_go_regiao_oeste_I) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_oeste_I)

future_forecast_tbl_regiao_oeste_I %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl_regiao_oeste_I %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_oeste_I <- 
  future_forecast_tbl_regiao_oeste_I %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_oeste_I <- 
  future_forecast_tbl_regiao_oeste_I %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "oeste I")




# regiao oeste II -----------------------------------------------------------------

nascimentos_go_regiao_oeste_II <- nascimentos_go %>% 
  filter(ds_nomepad == "OESTE II") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_regiao_oeste_II <- time_series_split(
  nascimentos_go_regiao_oeste_II,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_oeste_II %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_oeste_II

#----------------------------------------------------------------------------------
model_arima_regiao_oeste_II <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_oeste_II))

model_prophet_regiao_oeste_II <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_oeste_II))

model_fit_ets_regiao_oeste_II <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_oeste_II))

model_tbl_regiao_oeste_II <- modeltime_table(
  model_arima_regiao_oeste_II,
  model_prophet_regiao_oeste_II,
  model_fit_ets_regiao_oeste_II
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_oeste_II <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_oeste_II))

calib_tbl_regiao_oeste_II %>% modeltime_accuracy()

prophet_treino_regiao_oeste_II <- calib_tbl_regiao_oeste_II[[5]][[2]]

prophet_treino_regiao_oeste_II %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_oeste_II %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_oeste_II),
    actual_data = nascimentos_go_regiao_oeste_II
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_oeste_II <- calib_tbl_regiao_oeste_II %>% 
  modeltime_refit(nascimentos_go_regiao_oeste_II) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_oeste_II)

future_forecast_tbl_regiao_oeste_II %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl_regiao_oeste_II %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_oeste_II <- 
  future_forecast_tbl_regiao_oeste_II %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_oeste_II <- 
  future_forecast_tbl_regiao_oeste_II %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "oeste II")




# regiao sul -----------------------------------------------------------------

nascimentos_go_regiao_sul <- nascimentos_go %>% 
  filter(ds_nomepad == "SUL") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_sul <- time_series_split(
  nascimentos_go_regiao_sul,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_sul %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_sul

#----------------------------------------------------------------------------------
model_arima_regiao_sul <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sul))

model_prophet_regiao_sul <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sul))

model_fit_ets_regiao_sul <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_sul))

model_tbl_regiao_sul <- modeltime_table(
  model_arima_regiao_sul,
  model_prophet_regiao_sul,
  model_fit_ets_regiao_sul
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_sul <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_sul))

calib_tbl_regiao_sul %>% modeltime_accuracy()

prophet_treino_regiao_sul <- calib_tbl_regiao_sul[[5]][[2]]

prophet_treino_regiao_sul %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_sul %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_sul),
    actual_data = nascimentos_go_regiao_sul
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_sul <- calib_tbl_regiao_sul %>% 
  modeltime_refit(nascimentos_go_regiao_sul) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_sul)

future_forecast_tbl_regiao_sul %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



future_forecast_tbl_regiao_sul %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()


#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_sul <- 
  future_forecast_tbl_regiao_sul %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_sul <- 
  future_forecast_tbl_regiao_sul %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "sul")


# regiao sudoeste II-----------------------------------------------------------------

nascimentos_go_regiao_sudoeste_II <- nascimentos_go %>% 
  filter(ds_nomepad == "SUDOESTE II") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_sudoeste_II <- time_series_split(
  nascimentos_go_regiao_sudoeste_II,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_sudoeste_II %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_sudoeste_II

#----------------------------------------------------------------------------------
model_arima_regiao_sudoeste_II <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sudoeste_II))

model_prophet_regiao_sudoeste_II <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sudoeste_II))

model_fit_ets_regiao_sudoeste_II <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_sudoeste_II))

model_tbl_regiao_sudoeste_II <- modeltime_table(
  model_arima_regiao_sudoeste_II,
  model_prophet_regiao_sudoeste_II,
  model_fit_ets_regiao_sudoeste_II
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_sudoeste_II <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_sudoeste_II))

calib_tbl_regiao_sudoeste_II %>% modeltime_accuracy()

prophet_treino_regiao_sudoeste_II <- calib_tbl_regiao_sudoeste_II[[5]][[2]]

prophet_treino_regiao_sudoeste_II %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_sudoeste_II %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_sudoeste_II),
    actual_data = nascimentos_go_regiao_sudoeste_II
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_sudoeste_II <- calib_tbl_regiao_sudoeste_II %>% 
  modeltime_refit(nascimentos_go_regiao_sudoeste_II) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_sudoeste_II)

future_forecast_tbl_regiao_sudoeste_II %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)

future_forecast_tbl_regiao_sudoeste_II %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()


#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_sudoeste_II <- 
  future_forecast_tbl_regiao_sudoeste_II %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_sudoeste_II <- 
  future_forecast_tbl_regiao_sudoeste_II %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "Sudoeste II")


# regiao sudoeste I-----------------------------------------------------------------

nascimentos_go_regiao_sudoeste_I <- nascimentos_go %>% 
  filter(ds_nomepad == "SUDOESTE I") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_sudoeste_I <- time_series_split(
  nascimentos_go_regiao_sudoeste_I,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_sudoeste_I %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_sudoeste_I

#----------------------------------------------------------------------------------
model_arima_regiao_sudoeste_I <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sudoeste_I))

model_prophet_regiao_sudoeste_I <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sudoeste_I))

model_fit_ets_regiao_sudoeste_I <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_sudoeste_I))

model_tbl_regiao_sudoeste_I <- modeltime_table(
  model_arima_regiao_sudoeste_I,
  model_prophet_regiao_sudoeste_I,
  model_fit_ets_regiao_sudoeste_I
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_sudoeste_I <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_sudoeste_I))

calib_tbl_regiao_sudoeste_I %>% modeltime_accuracy()

prophet_treino_regiao_sudoeste_I <- calib_tbl_regiao_sudoeste_I[[5]][[2]]

prophet_treino_regiao_sudoeste_I %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_sudoeste_I %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_sudoeste_I),
    actual_data = nascimentos_go_regiao_sudoeste_I
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_sudoeste_I <- calib_tbl_regiao_sudoeste_I %>% 
  modeltime_refit(nascimentos_go_regiao_sudoeste_I) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_sudoeste_I)

future_forecast_tbl_regiao_sudoeste_I %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)

future_forecast_tbl_regiao_sudoeste_I %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()


#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_sudoeste_I <- 
  future_forecast_tbl_regiao_sudoeste_I %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_sudoeste_I <- 
  future_forecast_tbl_regiao_sudoeste_I %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "sudoeste I")


# regiao serra da mesa-----------------------------------------------------------------

nascimentos_go_regiao_serra_da_mesa <- nascimentos_go %>% 
  filter(ds_nomepad == "SERRA DA MESA") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_serra_da_mesa <- time_series_split(
  nascimentos_go_regiao_serra_da_mesa,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_serra_da_mesa %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_serra_da_mesa

#----------------------------------------------------------------------------------
model_arima_regiao_serra_da_mesa <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_serra_da_mesa))

model_prophet_regiao_serra_da_mesa <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_serra_da_mesa))

model_fit_ets_regiao_serra_da_mesa <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_serra_da_mesa))

model_tbl_regiao_serra_da_mesa <- modeltime_table(
  model_arima_regiao_serra_da_mesa,
  model_prophet_regiao_serra_da_mesa,
  model_fit_ets_regiao_serra_da_mesa
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_serra_da_mesa <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_serra_da_mesa))

calib_tbl_regiao_serra_da_mesa %>% modeltime_accuracy()

prophet_treino_regiao_serra_da_mesa <- calib_tbl_regiao_serra_da_mesa[[5]][[2]]

prophet_treino_regiao_serra_da_mesa %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_serra_da_mesa %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_serra_da_mesa),
    actual_data = nascimentos_go_regiao_serra_da_mesa
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_serra_da_mesa <- calib_tbl_regiao_serra_da_mesa %>% 
  modeltime_refit(nascimentos_go_regiao_serra_da_mesa) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_serra_da_mesa)

future_forecast_tbl_regiao_serra_da_mesa %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)

future_forecast_tbl_regiao_serra_da_mesa %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()


#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_serra_da_mesa <- 
  future_forecast_tbl_regiao_serra_da_mesa %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previs?o")


total_nascimentos_atual_recente_regiao_serra_da_mesa <- 
  future_forecast_tbl_regiao_serra_da_mesa %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "serra da mesa")


# regiao sao patricio II-----------------------------------------------------------------

nascimentos_go_regiao_sao_patricio_II<- nascimentos_go %>% 
  filter(ds_nomepad == "SAO PATRICIO II")  %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_sao_patricio_II <- time_series_split(
  nascimentos_go_regiao_sao_patricio_II,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_sao_patricio_II %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_sao_patricio_II

#----------------------------------------------------------------------------------
model_arima_regiao_sao_patricio_II <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sao_patricio_II))

model_prophet_regiao_sao_patricio_II <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sao_patricio_II))

model_fit_ets_regiao_sao_patricio_II <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_sao_patricio_II))

model_tbl_regiao_sao_patricio_II <- modeltime_table(
  model_arima_regiao_sao_patricio_II,
  model_prophet_regiao_sao_patricio_II,
  model_fit_ets_regiao_sao_patricio_II
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_sao_patricio_II <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_sao_patricio_II))

calib_tbl_regiao_sao_patricio_II %>% modeltime_accuracy()

prophet_treino_regiao_sao_patricio_II <- calib_tbl_regiao_sao_patricio_II[[5]][[2]]

prophet_treino_regiao_sao_patricio_II %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_sao_patricio_II %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_sao_patricio_II),
    actual_data = nascimentos_go_regiao_sao_patricio_II
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_sao_patricio_II <- calib_tbl_regiao_sao_patricio_II %>% 
  modeltime_refit(nascimentos_go_regiao_sao_patricio_II) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_sao_patricio_II)

future_forecast_tbl_regiao_sao_patricio_II %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)

future_forecast_tbl_regiao_sao_patricio_II %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()


#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_sao_patricio_II <- 
  future_forecast_tbl_regiao_sao_patricio_II %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_sao_patricio_II <- 
  future_forecast_tbl_regiao_sao_patricio_II %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "sao patricio II")

# regiao sao patricio I-----------------------------------------------------------------

nascimentos_go_regiao_sao_patricio_I<- nascimentos_go %>% 
  filter(ds_nomepad == "SAO PATRICIO I") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)

sp <- datas %>% 
  left_join(nascimentos_go_regiao_sao_patricio_I, by = "dtnasc") 

sp$total[is.na(sp$total)] <- 0


#----------------------------------------------------------------------------------
splits_regiao_sao_patricio_I <- time_series_split(
  sp,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_sao_patricio_I %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_sao_patricio_I

#----------------------------------------------------------------------------------
model_arima_regiao_sao_patricio_I <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sao_patricio_I))

model_prophet_regiao_sao_patricio_I <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_sao_patricio_I))

model_fit_ets_regiao_sao_patricio_I <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_sao_patricio_I))

model_tbl_regiao_sao_patricio_I <- modeltime_table(
  model_arima_regiao_sao_patricio_I,
  model_prophet_regiao_sao_patricio_I,
  model_fit_ets_regiao_sao_patricio_I
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_sao_patricio_I <- model_tbl_regiao_sao_patricio_I %>% 
  modeltime_calibrate(testing(splits_regiao_sao_patricio_I))

calib_tbl_regiao_sao_patricio_I %>% modeltime_accuracy()

prophet_treino_regiao_sao_patricio_I <- calib_tbl_regiao_sao_patricio_I[[5]][[2]]

prophet_treino_regiao_sao_patricio_I %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_sao_patricio_I %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_sao_patricio_I),
    actual_data = nascimentos_go_regiao_sao_patricio_I
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_sao_patricio_I <- calib_tbl_regiao_sao_patricio_I %>% 
  modeltime_refit(nascimentos_go_regiao_sao_patricio_I) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_sao_patricio_I)

future_forecast_tbl_regiao_sao_patricio_I %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)

future_forecast_tbl_regiao_sao_patricio_I %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()


#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_sao_patricio_I <- 
  future_forecast_tbl_regiao_sao_patricio_I %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previs?o")


total_nascimentos_atual_recente_regiao_sao_patricio_I <- 
  future_forecast_tbl_regiao_sao_patricio_I %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "sao patricio I")


# regiao rio vermelho -----------------------------------------------------------------

nascimentos_go_regiao_rio_vermelho <- nascimentos_go %>% 
  filter(ds_nomepad == "RIO VERMELHO") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_rio_vermelho <- time_series_split(
  nascimentos_go_regiao_rio_vermelho,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_rio_vermelho %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_rio_vermelho

#----------------------------------------------------------------------------------
model_arima_regiao_rio_vermelho <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_rio_vermelho))

model_prophet_regiao_rio_vermelho <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_rio_vermelho))

model_fit_ets_regiao_rio_vermelho <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_rio_vermelho))

model_tbl_regiao_rio_vermelho <- modeltime_table(
  model_arima_regiao_rio_vermelho,
  model_prophet_regiao_rio_vermelho,
  model_fit_ets_regiao_rio_vermelho
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_rio_vermelho <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_rio_vermelho))

calib_tbl_regiao_rio_vermelho %>% modeltime_accuracy()

prophet_treino_regiao_rio_vermelho <- calib_tbl_regiao_rio_vermelho[[5]][[2]]

prophet_treino_regiao_rio_vermelho %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_rio_vermelho %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_rio_vermelho),
    actual_data = nascimentos_go_regiao_rio_vermelho
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_rio_vermelho <- calib_tbl_regiao_rio_vermelho %>% 
  modeltime_refit(nascimentos_go_regiao_rio_vermelho) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_rio_vermelho)

future_forecast_tbl_regiao_rio_vermelho %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)

future_forecast_tbl_regiao_rio_vermelho %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()


#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_rio_vermelho <- 
  future_forecast_tbl_regiao_rio_vermelho %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_rio_vermelho <- 
  future_forecast_tbl_regiao_rio_vermelho %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "rio vermelho")



# regiao pirineus -----------------------------------------------------------------

nascimentos_go_regiao_pirineus <- nascimentos_go %>% 
  filter(ds_nomepad == "PIRINEUS") %>% 
  ungroup() %>% 
  select(-ds_nomepad, -co_regsaud)


#----------------------------------------------------------------------------------
splits_regiao_pirineus <- time_series_split(
  nascimentos_go_regiao_pirineus,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_pirineus %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(dtnasc, total)

splits_regiao_pirineus

#----------------------------------------------------------------------------------
model_arima_regiao_pirineus <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(total ~ dtnasc, training(splits_regiao_pirineus))

model_prophet_regiao_pirineus <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(total ~ dtnasc, training(splits_regiao_pirineus))

model_fit_ets_regiao_pirineus <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ dtnasc, data = training(splits_regiao_pirineus))

model_tbl_regiao_pirineus <- modeltime_table(
  model_arima_regiao_pirineus,
  model_prophet_regiao_pirineus,
  model_fit_ets_regiao_pirineus
)

#----------------------------------------------------------------------------------

calib_tbl_regiao_pirineus <- model_tbl %>% 
  modeltime_calibrate(testing(splits_regiao_pirineus))

calib_tbl_regiao_pirineus %>% modeltime_accuracy()

prophet_treino_regiao_pirineus <- calib_tbl_regiao_pirineus[[5]][[2]]

prophet_treino_regiao_pirineus %>% 
  ggplot(aes(x = dtnasc)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

#----------------------------------------------------------------------------------

calib_tbl_regiao_pirineus %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_pirineus),
    actual_data = nascimentos_go_regiao_pirineus
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_pirineus <- calib_tbl_regiao_pirineus %>% 
  modeltime_refit(nascimentos_go_regiao_pirineus) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = nascimentos_go_regiao_pirineus)

future_forecast_tbl_regiao_pirineus %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)

future_forecast_tbl_regiao_pirineus %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()


#----------------------------------------------------------------------------------

total_nascimentos_previsao_regiao_pirineus <- 
  future_forecast_tbl_regiao_pirineus %>% 
  filter(.key == "prediction" & .model_desc == "PROPHET") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "previsão")


total_nascimentos_atual_recente_regiao_pirineus <- 
  future_forecast_tbl_regiao_pirineus %>% 
  filter(.index > "2015-01-01") %>% 
  filter(.key == "actual") %>% 
  mutate(mes_ano = format(.index, "%Y-%m")) %>% 
  mutate(ano = year(.index)) %>% 
  group_by(mes_ano, ano) %>% 
  summarise(total = sum(.value)) %>% 
  mutate(tipo = "atual") %>% 
  mutate(regiao = "pirineus")







