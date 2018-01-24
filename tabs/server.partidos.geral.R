##-- + Atualizações dos filtros ----
##-- ++ Atualizações dos cargos ----
observeEvent(input$partido_geral_ano,{
  ano <- isolate(input$partido_geral_ano)

  if(!is.null(ano)){
    chaves_sub <- chaves %>%
      filter(ANO_ELEICAO == ano) %>%
      collect()

    ##-- Setando o cargo default
    cargos <- unique(chaves_sub$DESCRICAO_CARGO)
    cargo_default <- input$partido_geral_cargo

    if(!(cargo_default %in% cargos)){
      cargo_default <- cargos[1]
    }

    ##-- Atualizando os cargos ----
    updatePickerInput(session = session,
                      inputId = "partido_geral_cargo",
                      label = "Cargo",
                      # choices = unique(chaves_sub$DESCRICAO_CARGO),
                      choices = c("GOVERNADOR", "PRESIDENTE", "PREFEITO"), 
                      selected = cargo_default)

  }

}, priority = 1)
##-- ++ Atualizações dos turnos ----
observeEvent(c(input$partido_geral_ano,
               input$partido_geral_cargo),{

                 ano <- isolate(input$partido_geral_ano)
                 cargo <- isolate(input$partido_geral_cargo)

                 # if(!is.null(cargo)){
                 #   chaves_sub <- chaves %>%
                 #     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo) %>%
                 #     collect()
                 # 
                 #   ##-- Setando o cargo default
                 #   turnos <- unique(chaves_sub$NUM_TURNO)
                 #   turno_default <- input$partido_geral_turno
                 # 
                 #   if(!(turno_default %in% turnos)){
                 #     turno_default <- "1º turno"
                 #   }
                 # 
                 #   turnos <- paste0(turnos, "º turno")
                 # 
                 #   ##-- Atualizando os partidos ----
                 #   updatePickerInput(sessio = session,
                 #                     inputId = "partido_geral_turno",
                 #                     label = "Turno",
                 #                     choices = turnos,
                 #                     selected = turno_default)
                 # }

               }, priority = 2)
##-- ++ Atualizações dos partidos ----
observeEvent(c(input$partido_geral_ano,
               input$partido_geral_cargo),{

                 ano <- isolate(input$partido_geral_ano)
                 cargo <- isolate(input$partido_geral_cargo)
                 # turno <- isolate(input$partido_geral_turno)
                 # turno <- ifelse(turno == "1º turno", "1", "2")

                 # if(!is.null(cargo)){
                 #   chaves_sub <- chaves %>%
                 #     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo & NUM_TURNO == turno) %>%
                 #     collect()
                 # 
                 #   ##-- Setando o cargo default
                 #   partidos <- levels(factor(x = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO))),
                 #                             levels = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO)))))
                 #   partido_default <- input$partido_geral_partido
                 # 
                 #   if(!(partido_default %in% partidos)){
                 #     partido_default <- "Todos os partidos"
                 #   }
                 # 
                 #   ##-- Atualizando os partidos ----
                 #   updatePickerInput(session = session,
                 #                     inputId = "partido_geral_partido",
                 #                     label = "Partido",
                 #                     choices = partidos,
                 #                     selected = partido_default)
                 # }

               }, priority = 3)
##-- ++ Atualizações dos estados ----
observeEvent(c(input$partido_geral_ano,
               input$partido_geral_cargo,
               # input$partido_geral_turno,
               input$partido_geral_partido),{

                 ano <- isolate(input$partido_geral_ano)
                 cargo <- isolate(input$partido_geral_cargo)
                 # turno <- isolate(input$partido_geral_turno)
                 # turno <- ifelse(turno == "1º turno", "1", "2")
                 partido <- input$partido_geral_partido

                 if(!is.null(cargo)){

                   if(partido != "Todos os partidos"){
                     chaves_sub <- chaves %>%
                       filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo & NUM_TURNO == turno & SIGLA_PARTIDO == partido) %>%
                       collect()
                   } else{
                     chaves_sub <- chaves %>%
                       filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo) %>%
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
                     estado_default <- input$partido_geral_estado

                     if(!(estado_default %in% estados_opt)){
                       estado_default <- "Todos os estados"
                     }

                     if(cargo %in% c("PREFEITO", "VEREADOR")){
                       estado_default <- estados_opt[2]
                     }

                     updatePickerInput(session = session,
                                       inputId = "partido_geral_estado",
                                       label = "Estado",
                                       choices = opcoes,
                                       selected = estado_default)
                   } else{
                     ##-- Setando o cargo default
                     estados_opt <- levels(factor(x = c("Todos os estados", estados),
                                                  levels = c("Todos os estados", estados)))
                     estado_default <- input$partido_geral_estado

                     if(!(estado_default %in% estados_opt)){
                       estado_default <- "Todos os estados"
                     }

                     if(cargo %in% c("PREFEITO", "VEREADOR")){
                       estado_default <- estados_opt[2]
                     }

                     updatePickerInput(session = session,
                                       inputId = "partido_geral_estado",
                                       label = "Estado",
                                       choices = estados_opt,
                                       selected = estado_default)
                   }

                 }

               }, priority = 4)
##-- ++ Atualizações dos candidatos ----
observeEvent(c(input$partido_geral_ano,
               input$partido_geral_cargo,
               # input$partido_geral_turno,
               input$partido_geral_partido,
               input$partido_geral_estado),{

                 ano <- isolate(input$partido_geral_ano)
                 cargo <- isolate(input$partido_geral_cargo)
                 # turno <- isolate(input$partido_geral_turno)
                 # turno <- ifelse(turno == "1º turno", "1", "2")
                 partido <- isolate(input$partido_geral_partido)
                 estado <- isolate(input$partido_geral_estado)

                 if(!is.null(ano)){

                   if(!is.null(cargo)){

                     chaves_sub <- chaves %>%
                       filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo) %>%
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
                                         inputId = "partido_geral_cpf",
                                         label = "Candidato",
                                         choices = candidatos,
                                         selected = NULL)
                     }
                   }
                 }

               }, priority = 5)
##-- Grafios ----
base_treemap_desp <- eventReactive(input$partidos_gerar_visualizacoes1, {
  
  despesa_part_desp <- despesa_part %>%
    group_by(`Sigla  Partido`, `Tipo despesa`) %>%
    summarise(total_despesa_partido = sum(`Valor despesa`)) %>%
    ungroup() %>%
    group_by(`Sigla  Partido`) %>%
    collect() %>%
    mutate(total_partido = sum(total_despesa_partido),
           perc_despesa = total_despesa_partido/total_partido) %>%
    ungroup() %>%
    mutate(perc_partido = total_partido/sum(total_despesa_partido)) %>%
    as.data.frame()
  
  numeric_columns2 <- sapply(despesa_part_desp, mode) == 'numeric'
  
  despesa_part_desp[numeric_columns2] <-  round(despesa_part_desp[numeric_columns2], digits = 3)
  
  names(despesa_part_desp)[1] <- 'partido'
  
  despesa_part_desp$partido <- as.factor(despesa_part_desp$partido)
  
  return(list(despesas = despesa_part_desp))
})

base_pref_partido <- eventReactive(input$partidos_gerar_visualizacoes1, {
  
  ano <- isolate(input$partido_geral_ano)
  cargo <- isolate(input$partido_geral_cargo)
  
  # if(cargo == "VEREADOR") dados <- vereador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano)
  if(cargo == "PREFEITO") dados <- prefeito %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano)
  # if(cargo == "DEPUTADO ESTADUAL") dados <- dep_estadual %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano)
  if(cargo == "GOVERNADOR") dados <- governador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano)
  # if(cargo == "DEPUTADO FEDERAL") dados <- dep_federal %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano)
  # if(cargo == "SENADOR") dados <- senador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano)
  # if(cargo == "PRESIDENTE") dados <- presidente %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano)
  
  pref <- dados
  pref <- pref %>% mutate(ANO_ELEICAO = as.numeric(ANO_ELEICAO))

  shape <- subset(regMun, !is.na(NOME))
  
  eleitos <- pref %>% 
    filter(DESC_SIT_TOT_TURNO == 'ELEITO', ANO_ELEICAO == ano)%>%
    select(SIGLA_PARTIDO, COD_MUN_IBGE, NOME_MUNICIPIO, ANO_ELEICAO, DESCRICAO_ELEICAO)
  
  eleitos_map <- eleitos %>%
    group_by(COD_MUN_IBGE) %>%
    collect() %>%
    mutate(ordem = order(COD_MUN_IBGE))%>%
    filter(ordem == 1)
  
  names(eleitos_map)[2] <- "COD"
  
  teste <- eleitos_map %>%
    group_by(SIGLA_PARTIDO) %>%
    summarise(n = n()) %>%
    arrange(desc(n))%>%
    mutate(perc = n/ sum(n),
           perca = cumsum(perc),
           partidos = ifelse(perca <= 0.9, SIGLA_PARTIDO, 'Outros'))
  
  eleitos_map2 <- merge(eleitos_map, teste , by = "SIGLA_PARTIDO", all.x = T)
  eleitos_map2 <- merge(shape, eleitos_map2, by = "COD", all.x = T)
  
  eleitos_map2@data$partidos <- as.factor(eleitos_map2@data$partidos)
  
  return(list(eleitos_map2 = eleitos_map2))
  
})

dados_partido_geral <- eventReactive(input$partidos_gerar_visualizacoes1, {

  ano <- isolate(input$partido_geral_ano)
  cargo <- isolate(input$partido_geral_cargo)
  # turno <- input$partido_geral_turno

  # if(cargo == "VEREADOR") dados <- vereador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno)
  if(cargo == "PREFEITO") dados <- prefeito %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano)
  # if(cargo == "DEPUTADO ESTADUAL") dados <- dep_estadual %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno)
  if(cargo == "GOVERNADOR") dados <- governador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano)
  # if(cargo == "DEPUTADO FEDERAL") dados <- dep_federal %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno)
  # if(cargo == "SENADOR") dados <- senador %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno)
  # if(cargo == "PRESIDENTE") dados <- presidente %>% filter(DESCRICAO_CARGO == cargo & ANO_ELEICAO == ano & NUM_TURNO == turno)
  
  return(dados = dados)
})

base_coligacoes <- reactive({
  ano <- isolate(input$partido_geral_ano)
  
  pref <- dados_partido_geral()

  ano_fed <- c(1998, 2002, 2006, 2010, 2014)
  if(!ano %in% ano_fed)
  {
    pref <- pref %>%
      filter(ANO_ELEICAO == ano) %>%
      group_by(SIGLA_PARTIDO, COMPOSICAO_COLIGACAO) %>%
      summarise(tot = n_distinct(COD_MUN_IBGE)) %>% 
      collect()
    
    #partidos_vec <- unique(pref$SIGLA_PARTIDO)
    pref$id <- 1:dim(pref)[1]
    
    coligacoes <- strsplit(pref$COMPOSICAO_COLIGACAO, split = " / ")
    
    partidos_vec <- unique(unlist(coligacoes, use.names = FALSE))
    
    all_coligation <- lapply(coligacoes, function(x) expand.grid(x,x))
    
    all_coligation <- rbindlist(all_coligation)
    names(all_coligation) <- c("SIGLA_PARTIDO", "PARTIDO_COLIGACAO")
    all_coligation <- all_coligation[!all_coligation$SIGLA_PARTIDO == all_coligation$PARTIDO_COLIGACAO,]
    
    all_coligation <- all_coligation %>%
      group_by(SIGLA_PARTIDO, PARTIDO_COLIGACAO) %>%
      summarise(tot_coligacao = n()) %>%
      arrange(desc(tot_coligacao))
    
    ordem <- all_coligation %>%
      group_by(SIGLA_PARTIDO) %>%
      summarise(n = sum(tot_coligacao)) %>%
      arrange(n)%>%
      .$SIGLA_PARTIDO
    
    all_coligation <- ungroup(all_coligation)
    
    all_coligation$SIGLA_PARTIDO <- factor(all_coligation$SIGLA_PARTIDO, levels = ordem) 
    
    all_coligation$PARTIDO_COLIGACAO <- factor(all_coligation$PARTIDO_COLIGACAO, levels = ordem) 
    
    all_coligation$tooltip <- sprintf("%s - %s
                     Número de Coligações: %s <br>",
                                      all_coligation$SIGLA_PARTIDO,
                                      all_coligation$PARTIDO_COLIGACAO,
                                      all_coligation$tot_coligacao)
  }else{
    pref <- pref %>%
      filter(ANO_ELEICAO == ano) %>%
      group_by(SIGLA_PARTIDO, COMPOSICAO_COLIGACAO) %>%
      summarise(tot = n_distinct(UF)) %>%
      collect()
    
    #partidos_vec <- unique(pref$SIGLA_PARTIDO)
    pref$id <- 1:dim(pref)[1]
    
    coligacoes <- strsplit(pref$COMPOSICAO_COLIGACAO, split = " / ")
    
    partidos_vec <- unique(unlist(coligacoes, use.names = FALSE))
    
    all_coligation <- lapply(coligacoes, function(x) expand.grid(x,x))
    
    all_coligation <- rbindlist(all_coligation)
    names(all_coligation) <- c("SIGLA_PARTIDO", "PARTIDO_COLIGACAO")
    
    all_coligation <- all_coligation[!all_coligation$SIGLA_PARTIDO == all_coligation$PARTIDO_COLIGACAO,]
    
    all_coligation <- all_coligation %>%
      group_by(SIGLA_PARTIDO, PARTIDO_COLIGACAO) %>%
      summarise(tot_coligacao = n()) %>%
      arrange(desc(tot_coligacao))
    
    ordem <- all_coligation %>%
      group_by(SIGLA_PARTIDO) %>%
      summarise(n = sum(tot_coligacao)) %>%
      arrange(n)%>%
      .$SIGLA_PARTIDO
    all_coligation <- ungroup(all_coligation)
    
    all_coligation$SIGLA_PARTIDO <- factor(all_coligation$SIGLA_PARTIDO, levels = ordem) 
    
    all_coligation$PARTIDO_COLIGACAO <- factor(all_coligation$PARTIDO_COLIGACAO, levels = ordem) 
    
    
    all_coligation$tooltip <- sprintf("%s - %s
                     Número de Coligações: %s <br>",
                                      all_coligation$SIGLA_PARTIDO,
                                      all_coligation$PARTIDO_COLIGACAO,
                                      all_coligation$tot_coligacao) 
  }
  
  return(all_coligation = all_coligation)
})

base_receita <- reactive({

    doadores <- receita_part %>%
      group_by(`Sigla  Partido`,`Nome do doador`) %>%
      summarise(total_doador_partido = sum(`Valor receita`)) %>%
      arrange(`Sigla  Partido`, desc(total_doador_partido)) %>%
      ungroup() %>%
      group_by(`Sigla  Partido`) %>%
      collect() %>%
      mutate(perc_doador_partido = total_doador_partido/sum(total_doador_partido),
             perca_doador_partido = cumsum(perc_doador_partido),
             doador = ifelse(perca_doador_partido <= 0.75, `Nome do doador`, 'Outros'))%>%
      as.data.frame()
    
    names(doadores)[1] <- 'partido'
    
    doadores$doador[doadores$partido == doadores$doador] <- paste(doadores$doador[doadores$partido == doadores$doador],"-")
    
   return(doadores)
    
})

output$treemap_desp <- renderHighchart({
  input$partidos_gerar_visualizacoes1
  ano_filtro <- isolate(input$partido_geral_ano)

  if(ano_filtro != 2014) {
    return(NULL)
  }
  
  despesas <- base_treemap_desp()
  despesa_part_desp <- despesas$despesas
  
  hctreemap2(despesa_part_desp,
             group_vars = c('partido', 'Tipo despesa'),
             size_var = 'total_despesa_partido',
             color_var = 'total_despesa_partido',
             layoutAlgorithm = "squarified",
             levelIsConstant = F,
             allowDrillToNode = T,
             animationLimiti = 1000,
             dataLabels = list(enabled = F, color = 'black', shadow = F),
             levels = list(list(level = 1,
                                dataLabels = list(enabled = T))))%>%
    hc_colorAxis(minColor = brewer.pal(9, "YlOrBr")[1],
                 maxColor = brewer.pal(9, "YlOrBr")[9]) 
})

output$mapa_partidos_cid <- renderLeaflet({
  
  eleitos_map2 <- base_pref_partido()$eleitos_map2
  pop_up_text <- sprintf("<strong>%s - %s</strong><br>
                       <br>Ano: %s <br>",
                         eleitos_map2$SIGLA_PARTIDO,eleitos_map2$NOME, 
                         eleitos_map2$ANO_ELEICAO)
  
  palleta <- tableau_color_pal(palette = 'tableau20')(length(levels(eleitos_map2@data$partidos)))
  cor_fator <- colorFactor(palleta,levels = levels(eleitos_map2@data$partidos))
  
  leaflet(eleitos_map2) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = regUF, fillOpacity =  0, weight = 0.85, color = "#000000") %>%
    addPolygons(color = ~cor_fator(partidos), 
                weight = 0.1,
                popup = pop_up_text,
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)) %>%
    addLegend("bottomright", colors = palleta, labels = levels(eleitos_map2@data$partidos),
              opacity = 1)
})

output$heatmap_coligacoes <- renderPlotly({
  
  all_coligation <- base_coligacoes()
    
  palette <- brewer.pal(n = 9, name = 'YlOrRd')
  
  plot_ly(data = all_coligation,
          x = ~PARTIDO_COLIGACAO, 
          y = ~SIGLA_PARTIDO,
          z = ~tot_coligacao,
          type = "heatmap",
          colors = palette,
          hoverinfo = "text",
          text = ~tooltip,
          colorbar = list(title = "Nº Coligações")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = ""))
})

output$treemap_doa <- renderHighchart({
  input$partidos_gerar_visualizacoes1
  
  ano_filtro <- isolate(input$partido_geral_ano)
  
  if(ano_filtro != 2014) {
    return(NULL)
  }
  
  doadores <- base_receita()
  
  hctreemap2(doadores,
             group_vars = c('partido', 'doador'),
             size_var = 'total_doador_partido',
             color_var = 'total_doador_partido',
             layoutAlgorithm = "squarified",
             levelIsConstant = F,
             allowDrillToNode = T,
             animationLimiti = 1000,
             dataLabels = list(enabled = F, color = 'black', shadow = F),
             levels = list(list(level = 1,
                                dataLabels = list(enabled = T))))%>%
    hc_colorAxis(minColor = brewer.pal(9, "YlOrBr")[1],
                 maxColor = brewer.pal(9, "YlOrBr")[9]) 
})
  
  