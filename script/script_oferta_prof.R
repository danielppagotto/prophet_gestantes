library(tidyverse); library(lubridate); library(modeltime); library(tidymodels)
library(timetk); library(readxl); library(patchwork)

setwd("~/GitHub/prophet_gestantes/script")

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

oferta_t <- oferta %>% 
  mutate(mes_ano = ym(str_c(ano, mes)),
         ano = year(mes_ano),
         mes = month(mes_ano),
         semestre = if_else(mes < 7, "1","2"),
         semestre_ano = str_c(semestre, "/", ano)) %>% 
  left_join(municipios_macrorregiao_saude, by = c("codufmun" = "cod_municipio")) %>%
  group_by(ano, macrorregiao, nivel_atencao, categoria) %>% 
  summarise(total_fte40 = sum(quantidade)/12)

oferta_semestral %>% 
  filter(ano > 2015 & ano < 2022) %>%  
  filter(nivel_atencao == "APS") %>% 
  ggplot(aes(x = semestre_ano, y = qtd_semestral, col = macrorregiao)) + geom_line(group = 1) + 
  facet_wrap(~categoria) + theme_minimal()



# tratamentos iniciais ----------------------------------------------------


oferta_semestral <- oferta %>% 
              mutate(mes_ano = ym(str_c(ano, mes)),
                     ano = year(mes_ano),
                     mes = month(mes_ano),
                     semestre = if_else(mes < 7, "1","2"),
                     semestre_ano = str_c(semestre, "/", ano),
                     fte_liquido = (fte * 0.6),
                     fte_mes = fte_liquido * 4, 
                     quantidade_semanal = fte_liquido/40, 
                     quantidade_mensal = fte_liquido/160,
                     quantidade_mensal = case_when(nivel_atencao == "APS" ~ 0.12 * quantidade_mensal,
                                                   nivel_atencao == "Atenção Secundária" ~ 0.50 * quantidade_mensal)) %>% 
                     rename(fte_semanal = fte) %>% 
              filter(mes_ano > "2016-12-01" & mes_ano < "2022-01-01") %>% 
              left_join(municipios_macrorregiao_saude, by = c("codufmun" = "cod_municipio")) %>% 
              group_by(macrorregiao, semestre_ano, ano, nivel_atencao, categoria) %>% 
              summarise(fte_semestral = sum(fte_semanal),
                        qtd_semestral = sum(quantidade_mensal))




oferta_eaps <- oferta_semestral %>% 
  filter(categoria == "Enfermeiro" & nivel_atencao == "APS")

oferta_maps <- oferta_semestral %>% 
  filter(categoria == "Médico" & nivel_atencao == "APS")

oferta_eas <- oferta_semestral %>% 
  filter(categoria == "Enfermeiro" & nivel_atencao == "Atenção Secundária")

oferta_mas <- oferta_semestral %>% 
  filter(categoria == "Médico" & nivel_atencao == "Atenção Secundária")

# writexl::write_xlsx(oferta_eaps, "oferta_eaps.xlsx")
# writexl::write_xlsx(oferta_maps, "oferta_maps.xlsx")
# writexl::write_xlsx(oferta_maps, "oferta_eas.xlsx")
# writexl::write_xlsx(oferta_maps, "oferta_mas.xlsx")

# write.csv(oferta_t, "oferta_tratada.csv")              
# writexl::write_xlsx(oferta_t, "oferta_tratada.xlsx")

# APS ---------------------------------------------------------------------
# enfermeiros -------------------------------------------------------------


oferta_t <- oferta %>% 
  mutate(mes_ano = ym(str_c(ano, mes)),
         ano = year(mes_ano),
         mes = month(mes_ano),
         semestre = if_else(mes < 7, "1","2"),
         semestre_ano = str_c(semestre, "/", ano),
         fte_liquido = (fte * 0.6),
         fte_mes = fte_liquido * 4, 
         quantidade_semanal = fte_liquido/40, 
         quantidade_mensal = fte_liquido/160,
         quantidade_mensal = case_when(nivel_atencao == "APS" ~ 0.12 * quantidade_mensal,
                                       nivel_atencao == "Atenção Secundária" ~ 0.60 * quantidade_mensal)) %>% 
  rename(fte_semanal = fte) %>% 
  filter(mes_ano > "2016-12-01" & mes_ano < "2022-01-01") %>% 
  left_join(municipios_macrorregiao_saude, by = c("codufmun" = "cod_municipio")) %>% 
  group_by(macrorregiao, mes_ano, nivel_atencao, categoria) %>% 
  summarise(fte_mensal = sum(fte_mes),
            qtd_mensal = sum(quantidade_mensal))


oferta_enf_aps <- oferta_t %>%  
                    filter(categoria == "Enfermeiro" & nivel_atencao == "APS") 

e_aps <- oferta_enf_aps %>% 
  filter(mes_ano > "2015-12-01" & mes_ano < "2022-01-01") %>% 
  ungroup() %>% 
  ggplot(aes(mes_ano, qtd_mensal, col = macrorregiao)) + geom_line(size = 1) +
  theme_minimal() + ggtitle("Enfermeiros na APS") + theme(legend.position='none') + 
  xlab("Mês e ano") + ylab("Total") 

# Médicos  ----------------------------------------------------------------

oferta_medico_aps <- oferta_t %>%  
  filter(categoria == "Médico" & nivel_atencao == "APS") 


writexl::write_xlsx(oferta_medico_aps, "oferta_medicos_aps.xlsx")


oferta_medico_as <- oferta_t %>% 
  filter(categoria == "Médico" & nivel_atencao == "Atenção Secundária")


writexl::write_xlsx(oferta_medico_as, "oferta_medicos_aps.xlsx")


m_aps <- oferta_medico_aps %>% 
  filter(mes_ano > "2015-12-01" & mes_ano < "2022-01-01") %>% 
  ungroup() %>% 
  ggplot(aes(mes_ano, qtd_mensal, col = macrorregiao)) + geom_line(size = 1) + theme_minimal() + 
  ggtitle("Médicos na APS") + xlab("Mês e ano") + ylab("Total")


# SecundÃ¡rio --------------------------------------------------------------

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
  ggtitle("Médicos Obstétricos na Atenção Secundária") + xlab("Mês e ano") + ylab("Total") + 
  theme(legend.position='none')



(e_aps + m_aps)/(m_as)






oferta_enf_aps <- read_excel("oferta_enf_aps.xlsx", 
                             sheet = "Regiao_centro_sudeste", 
                             col_types = c("text", "date", "text", 
                                           "text", "text", "numeric"))



oferta_enf_aps %>% 
  ggplot(aes(mes_ano, qtd_mensal, col = cenario)) + geom_line() + theme_minimal()














