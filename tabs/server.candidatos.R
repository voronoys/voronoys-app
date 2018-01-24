##-- Consulta candidatos ----
##-- + Atualizações dos filtros ----
##-- ++ Atualizações dos cargos ----
observeEvent(input$perfil_candidato_ano,{
  ano <- isolate(input$perfil_candidato_ano)
  
  if(!is.null(ano)){
    chaves_sub <- chaves %>%
      filter(ANO_ELEICAO == ano) %>%
      collect()
    
    ##-- Setando o cargo default
    cargos <- unique(chaves_sub$DESCRICAO_CARGO)
    cargo_default <- input$perfil_candidato_cargo
    
    if(!(cargo_default %in% cargos)){
      cargo_default <- cargos[1]
    }
    
    ##-- Atualizando os cargos ----
    updatePickerInput(session = session,
                      inputId = "perfil_candidato_cargo",
                      label = "Cargo", 
                      choices = unique(chaves_sub$DESCRICAO_CARGO), 
                      selected = cargo_default)
    
  }
  
}, priority = 1)
##-- ++ Atualizações dos turnos ----
observeEvent(c(input$perfil_candidato_ano, 
               input$perfil_candidato_cargo),{
                 
                 ano <- isolate(input$perfil_candidato_ano)
                 cargo <- isolate(input$perfil_candidato_cargo)
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo) %>%
                     collect()
                   
                   ##-- Setando o cargo default
                   turnos <- unique(chaves_sub$NUM_TURNO)
                   turno_default <- input$perfil_candidato_turno
                   
                   if(!(turno_default %in% turnos)){
                     turno_default <- "1º turno"
                   }
                   
                   turnos <- paste0(turnos, "º turno")
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(sessio = session,
                                     inputId = "perfil_candidato_turno", 
                                     label = "Turno", 
                                     choices = turnos, 
                                     selected = turno_default)
                 }
                 
               }, priority = 2)
##-- ++ Atualizações dos partidos ----
observeEvent(c(input$perfil_candidato_ano, 
               input$perfil_candidato_cargo, 
               input$perfil_candidato_turno),{
                 
                 ano <- isolate(input$perfil_candidato_ano)
                 cargo <- isolate(input$perfil_candidato_cargo)
                 turno <- isolate(input$perfil_candidato_turno)
                 turno <- ifelse(turno == "1º turno", "1", "2")
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo & NUM_TURNO == turno) %>%
                     collect()
                   
                   ##-- Setando o cargo default
                   partidos <- levels(factor(x = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO))),
                                             levels = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO)))))
                   partido_default <- input$perfil_candidato_partido
                   
                   if(!(partido_default %in% partidos)){
                     partido_default <- "Todos os partidos"
                   }
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(session = session,
                                     inputId = "perfil_candidato_partido",
                                     label = "Partido", 
                                     choices = partidos, 
                                     selected = partido_default)  
                 }
                 
               }, priority = 3)
##-- ++ Atualizações dos estados ----
observeEvent(c(input$perfil_candidato_ano, 
               input$perfil_candidato_cargo, 
               input$perfil_candidato_turno, 
               input$perfil_candidato_partido),{
                 
                 ano <- isolate(input$perfil_candidato_ano)
                 cargo <- isolate(input$perfil_candidato_cargo)
                 turno <- isolate(input$perfil_candidato_turno)
                 turno <- ifelse(turno == "1º turno", "1", "2")
                 partido <- input$perfil_candidato_partido
                 
                 if(!is.null(cargo)){
                   
                   if(partido != "Todos os partidos"){
                     chaves_sub <- chaves %>%
                       filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo & NUM_TURNO == turno & SIGLA_PARTIDO == partido) %>%
                       collect()
                   } else{
                     chaves_sub <- chaves %>%
                       filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo & NUM_TURNO == turno) %>%
                       collect()
                   }
                   
                   ##-- Atualizando os estados ----
                   estados_base <- sort(unique(chaves_sub$UF))
                   
                   if(length(estados_base) == 0){
                     if(cargo == "PRESIDENTE"){
                       opcoes <- levels(factor(x = c("Todos os estados", estados),
                                               levels = c("Todos os estados", estados)))
                     } else{
                       opcoes <- "Todos os estados"
                     }
                     
                     ##-- Setando o cargo default
                     estados_opt <- opcoes
                     estado_default <- input$perfil_candidato_estado
                     
                     if(!(estado_default %in% estados_opt)){
                       estado_default <- "Todos os estados"
                     }
                     
                     if(cargo %in% c("PREFEITO", "VEREADOR")){
                       estado_default <- estados_opt[2]
                     }
                     
                     updatePickerInput(session = session,
                                       inputId = "perfil_candidato_estado", 
                                       label = "Estado", 
                                       choices = opcoes, 
                                       selected = estado_default)
                   } else{
                     ##-- Setando o cargo default
                     estados_opt <- levels(factor(x = c("Todos os estados", estados),
                                                  levels = c("Todos os estados", estados)))
                     estado_default <- input$perfil_candidato_estado
                     
                     if(!(estado_default %in% estados_opt)){
                       estado_default <- "Todos os estados"
                     }
                     
                     if(cargo %in% c("PREFEITO", "VEREADOR")){
                       estado_default <- estados_opt[2]
                     }
                     
                     updatePickerInput(session = session,
                                       inputId = "perfil_candidato_estado", 
                                       label = "Estado", 
                                       choices = estados_opt, 
                                       selected = estado_default)
                   }
                   
                 }
                 
               }, priority = 4)
##-- ++ Atualizações dos candidatos ----
observeEvent(c(input$perfil_candidato_ano, 
               input$perfil_candidato_cargo, 
               input$perfil_candidato_turno, 
               input$perfil_candidato_partido,
               input$perfil_candidato_estado),{
                 
                 ano <- isolate(input$perfil_candidato_ano)
                 cargo <- isolate(input$perfil_candidato_cargo)
                 turno <- isolate(input$perfil_candidato_turno)
                 turno <- ifelse(turno == "1º turno", "1", "2")
                 partido <- isolate(input$perfil_candidato_partido)
                 estado <- isolate(input$perfil_candidato_estado)
                 
                 if(!is.null(ano)){
                   
                   if(!is.null(cargo)){
                     
                     chaves_sub <- chaves %>%
                       filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo & NUM_TURNO == turno) %>%
                       collect()
                     
                     if(!is.null(partido)){
                       
                       if(partido != "Todos os partidos"){
                         chaves_sub <- chaves_sub %>%
                           filter(SIGLA_PARTIDO == partido)
                       }
                       
                       if(estado != "Todos os estados"){
                         if(cargo != "PRESIDENTE"){
                           chaves_sub <- chaves_sub %>%
                             filter(UF == estado)  
                         }
                       }
                       
                       ##-- Atualizando os candidatos ----
                       chaves_sub <- chaves_sub %>% arrange(NOME_CANDIDATO)
                       candidatos <- as.list(chaves_sub$CPF_CANDIDATO)
                       names(candidatos) <- chaves_sub$NOME_CANDIDATO
                       
                       updatePickerInput(session = session,
                                         inputId = "perfil_candidato_cpf", 
                                         label = "Candidato", 
                                         choices = candidatos, 
                                         selected = NULL)
                     }
                   }
                 }
                 
               }, priority = 5)
##-- + Dados do candidato e eleição selecionada ----
dados_candidato <- eventReactive(input$perfil_candidato_gerar_visualizacoes,{
  candidato <- isolate(input$perfil_candidato_cpf)
  
  chaves_candidato <- chaves %>% filter(CPF_CANDIDATO == candidato) %>% collect()
  cargos_candidato <- unique(toupper(chaves_candidato$DESCRICAO_CARGO))
  
  dados <- list()
  dados[["vereador"]] <- vereador %>% filter(CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO) %>% collect()
  dados[["prefeito"]] <- prefeito %>% filter(CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO)  %>% collect()
  dados[["dep_estadual"]] <- dep_estadual %>% filter(CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO)  %>% collect()
  dados[["governador"]] <- governador %>% filter(CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO)  %>% collect()
  dados[["dep_federal "]] <- dep_federal %>% filter(CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO)  %>% collect()
  dados[["senador"]] <- senador %>% filter(CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO)  %>% collect()
  dados[["presidente"]] <- presidente %>% filter(CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO)  %>% collect()
  
  dados <- bind_rows(dados)
  
  return(dados = dados)
})
dados_eleicao <- eventReactive(input$perfil_candidato_gerar_visualizacoes,{
  ano <- isolate(input$perfil_candidato_ano)
  cargo <- isolate(input$perfil_candidato_cargo)
  turno <- isolate(input$perfil_candidato_turno)
  turno <- ifelse(turno == "1º turno", "1", "2")
  
  if(cargo == "VEREADOR") dados <- vereador %>% filter(ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "PREFEITO") dados <- prefeito %>% filter(ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "DEPUTADO ESTADUAL") dados <- dep_estadual %>% filter(ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "GOVERNADOR") dados <- governador %>% filter(ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "DEPUTADO FEDERAL") dados <- dep_federal %>% filter(ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "SENADOR") dados <- senador %>% filter(ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  if(cargo == "PRESIDENTE") dados <- presidente %>% filter(ANO_ELEICAO == ano & NUM_TURNO == turno) %>% select(-COMPOSICAO_COLIGACAO)
  
  return(dados = dados)
})
dados_candidato_eleicao <- eventReactive(input$perfil_candidato_gerar_visualizacoes,{
  ano <- input$perfil_candidato_ano
  cargo <- input$perfil_candidato_cargo
  turno <- input$perfil_candidato_turno
  turno <- ifelse(turno == "1º turno", 1, 2)
  candidato <- input$perfil_candidato_cpf
  
  if(cargo == "VEREADOR") dados <- vereador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO) # %>% collect()
  if(cargo == "PREFEITO") dados <- prefeito %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO) # %>% collect()
  if(cargo == "DEPUTADO ESTADUAL") dados <- dep_estadual %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO) # %>% collect()
  if(cargo == "GOVERNADOR") dados <- governador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO) # %>% collect()
  if(cargo == "DEPUTADO FEDERAL") dados <- dep_federal %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO) # %>% collect()
  if(cargo == "SENADOR") dados <- senador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO) # %>% collect()
  if(cargo == "PRESIDENTE") dados <- presidente %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & CPF_CANDIDATO == candidato) %>% select(-COMPOSICAO_COLIGACAO) # %>% collect()
  
  dados <- dados %>% filter(NUM_TURNO == turno)
  
  return(dados = dados)
})
##-- + Outputs ----
output$perfil_candidatos_mapa <- renderLeaflet({
  
  candidato <- isolate(input$perfil_candidato_cpf)
  estado <- isolate(input$perfil_candidato_estado)
  cargo <- isolate(input$perfil_candidato_cargo)
  turno <- isolate(input$perfil_candidato_turno)
  
  paleta_col <- colorNumeric(palette = colorRamp(c("#ffffff", "#002c9b"), 
                                                 interpolate = "linear"), 
                             domain = c(0,1))
  
  if(cargo == "PRESIDENTE" & estado == "Todos os estados"){
    dados_eleicoes <- dados_eleicao() %>%
      collect() %>%
      mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>%
      group_by(UF, CPF_CANDIDATO) %>%
      summarise(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
      ungroup() %>%
      group_by(UF) %>%
      mutate(PROPORCAO = QTDE_VOTOS/sum(QTDE_VOTOS)) %>%
      ungroup() %>%
      select(UF, CPF_CANDIDATO, QTDE_VOTOS, PROPORCAO) %>%
      filter(CPF_CANDIDATO == candidato) %>%
      unique()
    
    shape <- regUF  
    
    names(shape)[1] <- "UF"
    names(shape)[3] <- "REGIAO_SIGLA"
    shape <- merge(shape, dados_eleicoes, all.x = T)
    
    names(shape@data) <- c("UF", "Estado", "Região sigla", "Região", "CPF", "Número de votos", "Proporção")
    
    cores <- colorNumeric(palette = paleta_col, domain = range(shape@data$`Proporção`))
    cores_poligonos <- cores(shape@data$`Proporção`)
    
    shape@data$`Número de votos` <- formatC(shape@data$`Número de votos`, big.mark = ".", decimal.mark = ",", format = "d")
    shape@data$`prop_numerica` <- shape@data$`Proporção`
    shape@data$`Proporção` <- paste(round(shape@data$Proporção*100, 2), "%")
    
    popup <- popupTable(shape@data, zcol = c("Estado", "Número de votos", "Proporção"))
    label <- paste(shape@data$UF, formatC(shape@data$`Número de votos`, big.mark = ".", decimal.mark = ",", format = "d"), sep = " - ")
  } else{
    
    dados_eleicoes <- dados_eleicao() %>%
      collect() %>%
      mutate(QTDE_VOTOS = as.numeric(QTDE_VOTOS)) %>%
      group_by(COD_MUN_IBGE) %>%
      mutate(PROPORCAO = QTDE_VOTOS/sum(QTDE_VOTOS)) %>%
      ungroup() %>%
      select(UF, COD_MUN_IBGE, CPF_CANDIDATO, QTDE_VOTOS, PROPORCAO) %>%
      filter(CPF_CANDIDATO == candidato) %>%
      unique()
    
    shape <- regMun
    
    if(estado != "Todos os estados"){
      shape <- subset(regMun, UF == estado) 
    } else{
      estados_filtro <- unique(dados_eleicoes$UF)
      shape <- subset(regMun, UF %in% estados_filtro) 
      
    }
    
    names(shape)[1] <- "COD_MUN_IBGE"
    shape <- merge(shape, dados_eleicoes, all.x = T)
    
    names(shape@data) <- c("COD_MUN_IBGE", "UF", "Município", "Região", "CPF", "Número de votos", "Proporção")
    shape@data$`Proporção`[which(is.na(shape@data$`Proporção`))] <- 0
    
    cores <- colorNumeric(palette = paleta_col, domain = range(shape@data$`Proporção`, na.rm = T))
    cores_poligonos <- cores(shape@data$`Proporção`)
    
    shape@data$`Número de votos` <- formatC(shape@data$`Número de votos`, big.mark = ".", decimal.mark = ",", format = "d")
    shape@data$`prop_numerica` <- shape@data$`Proporção`
    shape@data$`Proporção` <- paste(round(shape@data$Proporção*100, 2), "%")
    
    popup <- popupTable(shape@data, zcol = c("UF", "Município", "Número de votos", "Proporção"))
    label <- paste(shape$UF, shape$`Município`, formatC(shape$`Número de votos`, big.mark = ".", decimal.mark = ",", format = "d"), sep = " - ")
  }
  
  bbox_mun <- bbox(shape)
  
  map <- leaflet(data = shape) %>%
    addTiles() %>% 
    fitBounds(bbox_mun[1, 1], bbox_mun[2, 1], bbox_mun[1, 2], bbox_mun[2, 2])
  
  mapa_proporcoes <- map %>%
    addPolygons(data = regUF, fillOpacity = 0, weight = 0.75, color = "#222222") %>%
    addPolygons(stroke = T,
                color = "#bbbbbb", 
                group = shape$UF,
                label = label,
                popup = popup,
                weight = 0.5,
                smoothFactor = 0.1,
                fillOpacity = 0.7,
                fillColor = cores_poligonos,
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = T))
  
  mapa_proporcoes %>%
    addLegend("bottomright", pal = cores, values = shape$prop_numerica,
              opacity = .9, 
              title = "Proporção de votos", 
              labFormat = labelFormat(suffix= "%", digits = 4, transform = function(x) x*100))
})

output$perfil_candidato <- renderUI({
  
  ano <- isolate(input$perfil_candidato_ano)
  turno <- isolate(input$perfil_candidato_turno)
  turno <- ifelse(turno == "1º turno", "1", "2")
  
  dados_candidato_df <- dados_candidato() %>% 
    filter(ANO_ELEICAO == ano & NUM_TURNO == turno) %>%
    unique()
  
  info_gerais <- dados_candidato_df[1, c("NOME_CANDIDATO", "NUMERO_CANDIDATO", "NOME_URNA_CANDIDATO", "DESCRICAO_CARGO", 
                                         "SIGLA_PARTIDO", "NOME_PARTIDO", "DESCRICAO_OCUPACAO", "DATA_NASCIMENTO", 
                                         "DESCRICAO_SEXO", "DESCRICAO_GRAU_INSTRUCAO", "DESCRICAO_ESTADO_CIVIL",
                                         "DESCRICAO_COR_RACA", "DESCRICAO_NACIONALIDADE", "NOME_MUNICIPIO_NASCIMENTO", "DESC_SIT_TOT_TURNO")]
  
  info_gerais$IDADE <- year(as.period(interval(as.Date(info_gerais$DATA_NASCIMENTO, format = "%d/%m/%Y"), Sys.Date()), unit = "year"))
  
  n_votos <- formatC(sum(as.numeric(dados_candidato_df$QTDE_VOTOS)), format = "d", big.mark = ".", decimal.mark = ",")
  
  wellPanel(
    HTML(sprintf("<h3><b>Nome:</b> %s</h3>
                 <h3><b>Nome na urna:</b> %s</h3>
                 <h3><b>Sexo:</b> %s</h3>
                 <h3><b>Idade:</b> %s</h3>
                 <h3><b>Cidade natal:</b> %s</h3>
                 <h3><b>Profissão:</b> %s</h3>
                 <h3><b>Grau de instrução:</b> %s</h3>
                 <h3><b>Etnia:</b> %s</h3>
                 <h3><b>Partido:</b> %s</h3>
                 <h3><b>Cargo:</b> %s</h3>
                 <h3><b>Número de votos:</b> %s</h3>
                 <h3><b>Situação no turno:</b> %s</h3>",
                 info_gerais$NOME_CANDIDATO,
                 info_gerais$NOME_URNA_CANDIDATO,
                 info_gerais$DESCRICAO_SEXO,
                 info_gerais$IDADE,
                 info_gerais$NOME_MUNICIPIO_NASCIMENTO,
                 info_gerais$DESCRICAO_OCUPACAO,
                 info_gerais$DESCRICAO_GRAU_INSTRUCAO,
                 info_gerais$DESCRICAO_COR_RACA,
                 info_gerais$SIGLA_PARTIDO,
                 info_gerais$DESCRICAO_CARGO,
                 n_votos,
                 info_gerais$DESC_SIT_TOT_TURNO))
  )
  
})