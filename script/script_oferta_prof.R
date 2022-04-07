library(tidyverse); library(lubridate); library(modeltime); library(tidymodels)
library(timetk); library(readxl); library(patchwork)

setwd("~/GitHub/prophet_gestantes/bases") 

# municipio_regiao <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/municipio_regiao.csv", 
#                              col_types = cols(CO_MUNICIP = col_character(), 
#                                               CO_REGSAUD = col_character())) %>% 
#   janitor::clean_names()


# regioes_nomes <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/regioes_nomes.csv", 
#                           col_types = cols(CO_REGSAUD = col_character())) %>% 
#   janitor::clean_names() %>% 
#   select(co_regsaud, ds_nomepad)

municipios_macrorregiao_saude <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/municipios_macrorregiao_saude.csv") 

oferta <- read_csv("https://raw.githubusercontent.com/danielppagotto/prophet_gestantes/main/bases/oferta_profissionais.csv") %>% 
              janitor::clean_names() %>% 
              mutate(ano = as.character(ano))



# tratamentos iniciais ----------------------------------------------------


oferta_t <- oferta %>% 
              mutate(mes_ano = ym(str_c(ano, mes)),
                     fte_mes = fte * 4, 
                     quantidade_semanal = fte/40, 
                     quantidade_mensal = fte_mes/160) %>% 
              rename(fte_semanal = fte) %>% 
              left_join(municipios_macrorregiao_saude, by = c("codufmun" = "cod_municipio")) %>% 
              group_by(macrorregiao, mes_ano, nivel_atencao, categoria) %>% 
              summarise(fte_mensal = sum(fte_mes),
                        qtd_mensal = sum(quantidade_mensal))
              
write.csv(oferta_t, "oferta_tratada.csv")              

# APS ---------------------------------------------------------------------
# enfermeiros -------------------------------------------------------------

oferta_enf_aps <- oferta_t %>%  
                    filter(categoria == "Enfermeiro" & nivel_atencao == "APS") 

e_aps <- oferta_enf_aps %>% 
  filter(mes_ano > "2015-12-01" & mes_ano < "2022-01-01") %>% 
  ungroup() %>% 
  ggplot(aes(mes_ano, qtd_mensal, col = macrorregiao)) + geom_line(size = 1) + theme_minimal() + 
  ggtitle("Enfermeiros na APS") + theme(legend.position='none')

# médicos  ----------------------------------------------------------------

oferta_medico_aps <- oferta_t %>%  
  filter(categoria == "Médico" & nivel_atencao == "APS") 

m_aps <- oferta_medico_aps %>% 
  filter(mes_ano > "2015-12-01" & mes_ano < "2022-01-01") %>% 
  ungroup() %>% 
  ggplot(aes(mes_ano, qtd_mensal, col = macrorregiao)) + geom_line(size = 1) + theme_minimal() + 
  ggtitle("Médicos na APS") + xlab("Mês e ano") + ylab("Total")


# Secundário --------------------------------------------------------------

oferta_enf_sec <- oferta_t %>%  
  filter(categoria == "Enfermeiro" & nivel_atencao == "Atenção Secundária") 

e_as <- oferta_enf_sec %>% 
  filter(mes_ano > "2015-12-01" & mes_ano < "2022-01-01") %>% 
  ungroup() %>% 
  ggplot(aes(mes_ano, qtd_mensal, col = macrorregiao)) + geom_line(size = 1) + theme_minimal() +
  ggtitle("Enfermeiros obstétricos na Atenção Secundária") + xlab("Mês e ano") + ylab("Total") + 
  theme(legend.position='none')


oferta_medico_sec <- oferta_t %>%  
  filter(categoria == "Médico" & nivel_atencao == "Atenção Secundária") 

m_as <- oferta_medico_sec %>% 
  filter(mes_ano > "2015-12-01" & mes_ano < "2022-01-01") %>% 
  ungroup() %>% 
  ggplot(aes(mes_ano, qtd_mensal, col = macrorregiao)) + geom_line(size = 1) + theme_minimal() + 
  ggtitle("Médicos na Atenção Secundária") + xlab("Mês e ano") + ylab("Total") + 
  theme(legend.position='none')



(e_aps + m_aps)/(e_as + m_as)








# macrorregiao nordeste ---------------------------------------------------

macro_nordeste <- oferta_enf_aps %>% 
                      filter(macrorregiao == "Macrorregião Nordeste") %>% 
                      filter(mes_ano > "2007-12-01" & mes_ano < "2022-01-01") %>% 
                      ungroup()


splits_regiao_nordeste <- time_series_split(
  macro_nordeste,
  assess = "12 months",
  cumulative = TRUE
)

splits_regiao_nordeste %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(mes_ano, qtd_mensal)

splits_regiao_nordeste

model_arima_regiao_nordeste <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(qtd_mensal ~ mes_ano, training(splits_regiao_nordeste))

model_prophet_regiao_nordeste <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>% 
  fit(qtd_mensal ~ mes_ano, training(splits_regiao_nordeste))

model_fit_ets_regiao_nordeste <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(qtd_mensal ~ mes_ano, data = training(splits_regiao_nordeste))

model_tbl_regiao_nordeste <- modeltime_table(
  model_arima_regiao_nordeste,
  model_prophet_regiao_nordeste,
  model_fit_ets_regiao_nordeste
)

calib_tbl_regiao_nordeste <- model_tbl_regiao_nordeste %>% 
  modeltime_calibrate(testing(splits_regiao_nordeste))

calib_tbl_regiao_nordeste %>% modeltime_accuracy()

prophet_treino_regiao_nordeste <- calib_tbl_regiao_nordeste[[5]][[3]]

prophet_treino_regiao_nordeste %>% 
  ggplot(aes(x = mes_ano)) + geom_line(aes(y = .actual), col = "blue") +
  geom_line(aes(y = .prediction), col = "red") + theme_minimal()

calib_tbl_regiao_nordeste %>% 
  modeltime_forecast(
    new_data = testing(splits_regiao_nordeste),
    actual_data = macro_nordeste
  ) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_nordeste <- calib_tbl_regiao_nordeste %>% 
  modeltime_refit(macro_nordeste) %>% 
  modeltime_forecast(h = "42 months",
                     actual_data = macro_nordeste)

future_forecast_tbl_regiao_nordeste %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


future_forecast_tbl_regiao_nordeste %>% 
  filter(.model_desc == "PROPHET" | .model_desc == "ACTUAL") %>% 
  filter(.index > "2019-01-01") %>% 
  ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
  theme_minimal()

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















     
              
