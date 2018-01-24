dados_eleicao_geral <- reactive({
  ano <- input$eleicoes_ano
  cargo <- input$eleicoes_cargo
  turno <- input$eleicoes_turno
  turno <- ifelse(turno == "1º turno", "1", "2")
  
  if(cargo == "VEREADOR") dados <- vereador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "PREFEITO") dados <- prefeito %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "DEPUTADO ESTADUAL") dados <- dep_estadual %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "GOVERNADOR") dados <- governador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "DEPUTADO FEDERAL") dados <- dep_federal %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "SENADOR") dados <- senador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "PRESIDENTE") dados <- presidente %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  
  return(dados = dados)
})

output$mapa_uf_geral <- renderLeaflet({
  
  cargo <- input$eleicoes_cargo
  
  names(regUF)[c(1, 3)] <- c("UF", "REG")
  
  dados <- dados_eleicao_geral()
  dados <- dados %>% mutate(QTDE_VOTOS := as.numeric(QTDE_VOTOS)) %>% collect()
  
  paleta_col <- ggthemes::tableau_color_pal("tableau20")(20)
  
  ano_eleicao <- input$eleicoes_ano
  cod_uf <- input$eleicoes_estado
  turno <- input$eleicoes_turno
  turno <- ifelse(turno == "1º turno", "1", "2")
  
  mapa_uf(data = dados, poly = regUF, ano_eleicao = ano_eleicao, turno_filtro = turno)
})

output$mapa_mun_geral <- renderLeaflet({
  
  cargo <- input$eleicoes_cargo
  
  dados <- dados_eleicao_geral()
  dados <- dados %>% mutate(QTDE_VOTOS := as.numeric(QTDE_VOTOS)) %>% collect() %>% unique()
  
  paleta_col <- ggthemes::tableau_color_pal("tableau20")(20)
  
  ano_eleicao <- input$eleicoes_ano
  cod_uf <- input$eleicoes_estado
  if(cod_uf == "Todos os estados") cod_uf <- NULL
  turno <- input$eleicoes_turno
  turno <- ifelse(turno == "1º turno", "1", "2")
  
  mapa_mun(data = dados, poly = regMun, ano_eleicao = ano_eleicao, turno_filtro = turno, uf = cod_uf)
})

# output$donut_geral <- renderPlotly({
#   
#   cargo <- input$eleicoes_cargo
#   
#   dados <- dados_eleicao_geral()
#   dados <- dados %>% mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>% collect() %>% unique()
#   
#   paleta_col <- ggthemes::tableau_color_pal("tableau20")(20)
#   
#   ano_eleicao <- input$eleicoes_ano
#   cod_uf <- input$eleicoes_estado
#   turno <- input$eleicoes_turno
#   turno <- ifelse(turno == "1º turno", "1", "2")
#   
#   if(cod_uf == "Todos os estados"){
#     dados <- dados %>% filter(ANO_ELEICAO == ano_eleicao)
#     candidato_df <- dados %>%
#       group_by(NUM_TURNO, CPF_CANDIDATO, NUMERO_CANDIDATO, NOME_URNA_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO) %>%
#       summarise(QTDE_VOTOS_TOT = sum(QTDE_VOTOS))
#   } else{ 
#     dados <- dados %>% filter(ANO_ELEICAO == ano_eleicao & UF == cod_uf)
#     candidato_df <- dados %>%
#       group_by(NUM_TURNO, CPF_CANDIDATO, NUMERO_CANDIDATO, NOME_URNA_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO) %>%
#       summarise(QTDE_VOTOS_TOT = sum(QTDE_VOTOS))
#   }
#   
#   ## Donut da proporção de votos
#   donut_plot(data = candidato_df, 
#              value_var = "QTDE_VOTOS_TOT", 
#              group_var = "NOME_URNA_CANDIDATO", 
#              turno_filtro = turno, 
#              colors_pal = paleta_col) 
#   
# })

output$barras_geral <- renderPlotly({
  
  cargo <- input$eleicoes_cargo
  
  dados <- dados_eleicao_geral()
  dados <- dados %>% mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>% collect() %>% unique()
  
  paleta_col <- ggthemes::tableau_color_pal("tableau20")(20)
  
  ano_eleicao <- input$eleicoes_ano
  cod_uf <- input$eleicoes_estado
  turno <- input$eleicoes_turno
  turno <- ifelse(turno == "1º turno", "1", "2")
  
  if(cod_uf == "Todos os estados"){
    dados <- dados %>% filter(ANO_ELEICAO == ano_eleicao)
    candidato_df <- dados %>%
      group_by(NUM_TURNO, CPF_CANDIDATO, NUMERO_CANDIDATO, NOME_URNA_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO) %>%
      summarise(QTDE_VOTOS_TOT = sum(QTDE_VOTOS))
  } else{ 
    dados <- dados %>% filter(ANO_ELEICAO == ano_eleicao & UF == cod_uf)
    candidato_df <- dados %>%
      group_by(NUM_TURNO, CPF_CANDIDATO, NUMERO_CANDIDATO, NOME_URNA_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO) %>%
      summarise(QTDE_VOTOS_TOT = sum(QTDE_VOTOS))
  }
  
  ## Donut da proporção de votos
  bar_plot(data = candidato_df, 
           value_var = "QTDE_VOTOS_TOT", 
           group_var = "NOME_URNA_CANDIDATO", 
           turno_filtro = turno, 
           colors_pal = paleta_col) 
  
})