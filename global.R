##-- Pacotes ----
library(cepespR)
library(dplyr)
library(dbplyr)
library(data.table)
library(reshape2)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggthemes)
library(RColorBrewer)
library(sf)
library(sp)
library(scales)
library(leaflet)
library(plotly)
#devtools::install_github("jbkunst/highcharter")
library(highcharter)
library(DT)
library(mapview)
#devtools::install_github("lgsilvaesilva/mapsBR")
library(mapsBR)
library(deldir)
##-- Chamando as funções criadas ----
source("functions/utils.R")
source("functions/plot_functions.R")
source("carrega_dados.R")
cores <- c("#098ebb", "#fdc23a", "#e96449", "#818286")
##-- Chamando os componentes do header shiny ----
tab_files <- list.files(path = "tabs", full.names = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")]

anos <- chaves %>% distinct(ANO_ELEICAO) %>% collect() %>% .$ANO_ELEICAO
cargos <- chaves %>% distinct(DESCRICAO_CARGO) %>% collect() %>% .$DESCRICAO_CARGO
partidos <- chaves %>% distinct(SIGLA_PARTIDO) %>% collect() %>% .$SIGLA_PARTIDO
partidos <- sort(partidos)
estados <- chaves %>% distinct(UF) %>% collect() %>% .$UF
estados <- estados[!is.na(estados)]
municipios_df <- chaves %>% filter(UF == "SP") %>% distinct(COD_MUN_IBGE, NOME_MUNICIPIO) %>% collect() %>% group_by(COD_MUN_IBGE) %>% summarise(NOME_MUNICIPIO = last(NOME_MUNICIPIO))
municipios <- as.list(c(municipios_df$COD_MUN_IBGE, "TODOS MUNICIPIOS"))
names(municipios) <- c(municipios_df$NOME_MUNICIPIO, "Todos os municípios")

suppressMessages(lapply(tab_files, source))
##-- Chamando os shapes do mapsBR ----
data("regMun")
data("regUF")