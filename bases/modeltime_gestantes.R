library(tidyverse); library(lubridate); library(modeltime); library(tidymodels)
library(timetk)

setwd("~/GitHub/prophet_gestantes/bases")

nascimentos <- read_csv("nascimentos.csv") %>% select(-X1) %>% janitor::clean_names()

nascimentos$dtnasc <- dmy(nascimentos$dtnasc)

# nascimentos$mes_ano <- zoo::as.yearmon(nascimentos$dtnasc, "%Y %m")
nascimentos$mes_ano <- as.Date(format(nascimentos$dtnasc, "%Y-%m"))


nascimentos_go <- nascimentos %>% 
                    filter(dtnasc < "2021-10-01") %>% 
                    group_by(uf, dtnasc) %>%
                    summarise(total = sum(contagem))

nascimentos_go %>% 
  ggplot(aes(x = mes_ano, y = total)) + geom_line(size = 0.60) + 
  theme_minimal()

nascimentos_go %>% 
  plot_time_series(dtnasc, total)


# split e prophet ---------------------------------------------------------

splits <- time_series_split(
  nascimentos_go,
  assess = "6 months",
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
  modeltime_forecast(h = "24 months",
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


writexl::write_xlsx(total_nascimentos_previsao, "totalnascimentos_go.xlsx")

total_nascimentos <- rbind(total_nascimentos_atual_recente, total_nascimentos_previsao)


total_nascimentos %>% 
  ggplot(aes(mes_ano, total,  group = 1, col = tipo)) + geom_line() + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

