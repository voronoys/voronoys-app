##-- + Atualizações dos filtros ----
##-- ++ Atualizações dos cargos ----
observeEvent(input$partido_ano,{
  ano <- isolate(input$partido_ano)
  
  if(!is.null(ano)){
    chaves_sub <- chaves %>%
      filter(ANO_ELEICAO == ano) %>%
      collect()
    
    ##-- Setando o cargo default
    cargos <- unique(chaves_sub$DESCRICAO_CARGO)
    cargo_default <- input$partido_cargo
    
    if(!(cargo_default %in% cargos)){
      cargo_default <- cargos[1]
    }
    
    ##-- Atualizando os cargos ----
    updatePickerInput(session = session,
                      inputId = "partido_cargo",
                      label = "Cargo",
                      # choices = unique(chaves_sub$DESCRICAO_CARGO),
                      choices = c("GOVERNADOR", "PRESIDENTE", "PREFEITO"), 
                      selected = cargo_default)
    
  }
  
}, priority = 1)
##-- ++ Atualizações dos turnos ----
observeEvent(c(input$partido_ano,
               input$partido_cargo),{
                 
                 ano <- isolate(input$partido_ano)
                 cargo <- isolate(input$partido_cargo)
                 
                 # if(!is.null(cargo)){
                 #   chaves_sub <- chaves %>%
                 #     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo) %>%
                 #     collect()
                 # 
                 #   ##-- Setando o cargo default
                 #   turnos <- unique(chaves_sub$NUM_TURNO)
                 #   turno_default <- input$partido_turno
                 # 
                 #   if(!(turno_default %in% turnos)){
                 #     turno_default <- "1º turno"
                 #   }
                 # 
                 #   turnos <- paste0(turnos, "º turno")
                 # 
                 #   ##-- Atualizando os partidos ----
                 #   updatePickerInput(sessio = session,
                 #                     inputId = "partido_turno",
                 #                     label = "Turno",
                 #                     choices = turnos,
                 #                     selected = turno_default)
                 # }
                 
               }, priority = 2)
##-- ++ Atualizações dos partidos ----
observeEvent(c(input$partido_ano,
               input$partido_cargo),{
                 
                 ano <- isolate(input$partido_ano)
                 cargo <- isolate(input$partido_cargo)
                 # turno <- isolate(input$partido_turno)
                 # turno <- ifelse(turno == "1º turno", "1", "2")
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo) %>%
                     collect()
                   
                   ##-- Setando o cargo default
                   partidos <- levels(factor(x = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO))),
                                             levels = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO)))))
                   partido_default <- input$partido_partido
                   
                   if(!(partido_default %in% partidos)){
                     partido_default <- "Todos os partidos"
                   }
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(session = session,
                                     inputId = "partido_partido",
                                     label = "Partido",
                                     choices = partidos,
                                     selected = partido_default)
                 }
                 
               }, priority = 3)
##-- ++ Atualizações dos estados ----
observeEvent(c(input$partido_ano,
               input$partido_cargo,
               # input$partido_turno,
               input$partido_partido),{
                 
                 ano <- isolate(input$partido_ano)
                 cargo <- isolate(input$partido_cargo)
                 # turno <- isolate(input$partido_turno)
                 # turno <- ifelse(turno == "1º turno", "1", "2")
                 partido <- input$partido_partido
                 
                 if(!is.null(cargo)){
                   
                   if(partido != "Todos os partidos"){
                     chaves_sub <- chaves %>%
                       filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo & SIGLA_PARTIDO == partido) %>%
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
                     estado_default <- input$partido_estado
                     
                     if(!(estado_default %in% estados_opt)){
                       estado_default <- "Todos os estados"
                     }
                     
                     if(cargo %in% c("PREFEITO", "VEREADOR")){
                       estado_default <- estados_opt[2]
                     }
                     
                     updatePickerInput(session = session,
                                       inputId = "partido_estado",
                                       label = "Estado",
                                       choices = opcoes,
                                       selected = estado_default)
                   } else{
                     ##-- Setando o cargo default
                     estados_opt <- levels(factor(x = c("Todos os estados", estados),
                                                  levels = c("Todos os estados", estados)))
                     estado_default <- input$partido_estado
                     
                     if(!(estado_default %in% estados_opt)){
                       estado_default <- "Todos os estados"
                     }
                     
                     if(cargo %in% c("PREFEITO", "VEREADOR")){
                       estado_default <- estados_opt[2]
                     }
                     
                     updatePickerInput(session = session,
                                       inputId = "partido_estado",
                                       label = "Estado",
                                       choices = estados_opt,
                                       selected = estado_default)
                   }
                   
                 }
                 
               }, priority = 4)
##-- ++ Atualizações dos candidatos ----
observeEvent(c(input$partido_ano,
               input$partido_cargo,
               # input$partido_turno,
               input$partido_partido,
               input$partido_estado),{
                 
                 ano <- isolate(input$partido_ano)
                 cargo <- isolate(input$partido_cargo)
                 # turno <- isolate(input$partido_turno)
                 # turno <- ifelse(turno == "1º turno", "1", "2")
                 partido <- isolate(input$partido_partido)
                 estado <- isolate(input$partido_estado)
                 
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
                                         inputId = "partido_cpf",
                                         label = "Candidato",
                                         choices = candidatos,
                                         selected = NULL)
                     }
                   }
                 }
                 
               }, priority = 5)

##-- Graficos ----
base_rank_setor <- reactive({
  partido <- input$partido_partido
  
  doadores_setor <- receita_part %>%
    group_by(`Sigla  Partido`,`Setor econômico do doador`) %>%
    summarise(total_setor_partido = sum(`Valor receita`)) %>%
    arrange(`Sigla  Partido`, desc(total_setor_partido)) %>%
    ungroup() %>%
    group_by(`Sigla  Partido`) %>%
    collect() %>%
    mutate(perc_setor_partido = total_setor_partido/sum(total_setor_partido),
           perca_setor_partido = cumsum(perc_setor_partido)) %>%
    filter(`Sigla  Partido` == partido) %>%
    as.data.frame()
  
  names(doadores_setor)[2] <- "setor"
  names(doadores_setor)[1] <- "partido"
  
  doadores_setor$setor <- factor(doadores_setor$setor, levels = unique(doadores_setor$setor)[order(doadores_setor$total_setor_partido, decreasing = F)])
  levels(doadores_setor$setor)[levels(doadores_setor$setor) == "#NULO"] <- "Não Classificado"
  
  return(doadores_setor)
})

output$rank_setor <- renderPlotly({
  
  doadores_setor <- base_rank_setor()
  
  tooltip <- paste0("<b>",doadores_setor$partido[1:10],"</b><br>",
                    "Setor: ",doadores_setor$setor[1:10],"<br>",
                    "R$",paste0(round(doadores_setor$total_setor_partido[1:10]/1e6, digits = 2), "M"),"<br>")
  
  
  paleta <- tableau_color_pal("tableau20")(20)
  plot_ly(doadores_setor[1:10,],
          x = ~total_setor_partido,
          y = ~setor,
          color = ~as.character(setor),
          colors = paleta, 
          type = 'bar',
          orientation = 'h',
          text = tooltip,
          hoverinfo="text")%>%
    layout(xaxis = list(title = "Valor total de Doação"),
           yaxis = list(title = "", showticklabels = F),
           barmode = 'group',
           margin = list(l = 10))
  
})

base_ncand <- reactive({
  ano <- input$partido_ano
  cargo <- input$partido_cargo
  partido <- input$partido_partido
  
  if(cargo == "VEREADOR") consulta <- vereador %>% filter(ANO_ELEICAO == ano)
  if(cargo == "PREFEITO") consulta <- prefeito %>% filter(ANO_ELEICAO == ano)
  if(cargo == "DEPUTADO ESTADUAL") consulta <- dep_estadual %>% filter(ANO_ELEICAO == ano)
  if(cargo == "GOVERNADOR") consulta <- governador %>% filter(ANO_ELEICAO == ano)
  if(cargo == "DEPUTADO FEDERAL") consulta <- dep_federal %>% filter(ANO_ELEICAO == ano)
  if(cargo == "SENADOR") consulta <- senador %>% filter(ANO_ELEICAO == ano)
  if(cargo == "PRESIDENTE") consulta <- presidente %>% filter(ANO_ELEICAO == ano)
  
  data("regUF")
  n_cand <- consulta %>% 
    mutate(DESCRICAO_CARGO = toupper(DESCRICAO_CARGO)) %>%
    filter(ANO_ELEICAO == ano)%>%
    group_by(SIGLA_PARTIDO, DESCRICAO_CARGO, UF, ANO_ELEICAO)%>%
    collect() %>%
    summarise(n = n(),
              N_ELEI = sum(DESC_SIT_TOT_TURNO == 'ELEITO'))%>%
    ungroup()%>%
    group_by(UF)%>%
    mutate(cand_state = sum(n),
           prop_cand = n/cand_state)%>%
    filter(SIGLA_PARTIDO == partido, DESCRICAO_CARGO == cargo)
  
  base <- merge(regUF, n_cand, by.x = "COD", by.y = "UF")
  
  return(base)
})

output$mapa_cand <- renderLeaflet({
  
  base <- base_ncand()
  pal <- colorBin("YlOrRd", domain = base$n, bins = 10)
  
  pop_up_text <- sprintf("<strong>%s - %s</strong><br>
                         <br>Ano: %s <br>
                         <br>Cargo: %s <br>
                         <br>Total Candidatos: %s <br>
                         <br>Candidatos pelo partido: %s <br>
                         <br>Nº Eleitos: %s",
                         base$SIGLA_PARTIDO,base$COD, 
                         base$ANO_ELEICAO,
                         base$DESCRICAO_CARGO,
                         base$cand_state,
                         base$n,
                         base$N_ELEI)
  
  leaflet(base) %>%
    addProviderTiles(providers$CartoDB.Positron)%>%
    addPolygons(color = "#444444", 
                weight = 0.5,
                popup = pop_up_text,
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.5,
                fillColor = ~colorNumeric("YlOrRd", prop_cand)(prop_cand),
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE))   
  
})

donut <- reactive({
  ano <- input$partido_ano
  partido <- input$partido_partido
  
  est <- dep_estadual %>% filter(ANO_ELEICAO == ano) %>% collect()
  fed <- dep_federal %>% filter(ANO_ELEICAO == ano)  %>% collect()
  gov <- governador %>% filter(ANO_ELEICAO == ano)  %>% collect()
  pres <- presidente %>% filter(ANO_ELEICAO == ano)  %>% collect()
  sen <- senador %>% filter(ANO_ELEICAO == ano) %>% collect()
  
  dados_cand_part <- bind_rows(est, fed, gov, pres, sen)
  
  rm(est, fed, gov, pres, sen)
  
  return(dados_cand_part)
  
})

output$donut_sexo <- renderPlotly({
  partido <- input$partido_partido

  dados_cand_part <- donut()
  
  sex_cand_par <- dados_cand_part %>%
    group_by(SIGLA_PARTIDO, DESCRICAO_SEXO)%>%
    summarise(n = n(),
              N_ELEI = sum(DESC_SIT_TOT_TURNO == 'ELEITO'))%>%
    arrange(desc(n))%>%
    mutate(percn = n/sum(n),
           percn_acum = cumsum(percn),
           percn_label = paste0(100*round(percn, digits = 3), " %"),
           percel = N_ELEI/sum(N_ELEI),
           percel_acum = cumsum(percel),
           percel_label = paste0(100*round(percel, digits = 3), " %"))%>%
    as.data.frame()%>%
    filter(SIGLA_PARTIDO == partido)
  
  tooltip_sexo <-paste0("<b>",sex_cand_par$DESCRICAO_SEXO,"</b><br>",
                        "Nº Candidatos: ",sex_cand_par$n,"<br>",
                        "Nº Candidatos Eleitos:",sex_cand_par$N_ELEI,"<br>",
                        "Percentual de Candidatos: ",sex_cand_par$percn_label,"<br>")
  
  
  plot_ly(data = sex_cand_par, 
          labels = ~DESCRICAO_SEXO,
          values = ~n,
          hovertext = tooltip_sexo,
          hoverinfo="text") %>%
    add_pie(hole = 0.6) %>%
    layout(showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
})

output$donut_raca <- renderPlotly({
  partido <- input$partido_partido
  
  dados_cand_part <- donut()
  
  raca_cand_par <- dados_cand_part %>%
    group_by(SIGLA_PARTIDO, DESCRICAO_COR_RACA)%>%
    summarise(n = n(),
              N_ELEI = sum(DESC_SIT_TOT_TURNO == 'ELEITO'))%>%
    arrange(desc(n))%>%
    mutate(percn = n/sum(n),
           percn_acum = cumsum(percn),
           percn_label = paste0(100*round(percn, digits = 3), " %"),
           percel = N_ELEI/sum(N_ELEI),
           percel_acum = cumsum(percel),
           percel_label = paste0(100*round(percel, digits = 3), " %"))%>%
    as.data.frame()%>%
    filter(SIGLA_PARTIDO == partido)
  
  
  tooltip_raca <-paste0("<b>",raca_cand_par$DESCRICAO_COR_RACA,"</b><br>",
                        "Nº Candidatos: ",raca_cand_par$n,"<br>",
                        "Nº Candidatos Eleitos:",raca_cand_par$N_ELEI,"<br>",
                        "Percentual de Candidatos: ",raca_cand_par$percn_label,"<br>")
  
  plot_ly(data = raca_cand_par,
          labels = ~DESCRICAO_COR_RACA,
          values = ~n,
          hovertext = tooltip_raca,
          hoverinfo="text") %>%
    add_pie(hole = 0.6) %>%
    layout(showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
})

output$tabela <- renderDataTable({
  partido <- input$partido_partido
  
  dados_cand_part <- donut()
  
  inst_cand_par <- dados_cand_part %>%
    group_by(SIGLA_PARTIDO, DESCRICAO_GRAU_INSTRUCAO)%>%
    summarise(n = n(),
              N_ELEI = sum(DESC_SIT_TOT_TURNO == 'ELEITO'))%>%
    arrange(desc(n))%>%
    mutate(percn = n/sum(n),
           percn_acum = cumsum(percn),
           percn_label = paste0(100*round(percn, digits = 3), " %"),
           percel = N_ELEI/sum(N_ELEI),
           percel_label = paste0(100*round(percel, digits = 3), " %"))%>%
    ungroup()%>%
    mutate(percn_pad = scale(percn)) %>%
    as.data.frame()
  
  inst_cand_par_tab <- inst_cand_par %>%
    select(SIGLA_PARTIDO, DESCRICAO_GRAU_INSTRUCAO, n,percn_label, N_ELEI)%>%
    filter(SIGLA_PARTIDO == partido)
  
  datatable(inst_cand_par_tab,
            colnames = c("Partido", "Instrução", "Nº Cand", "Prop. Cand", "Nº Eleitos"),
            options = list(pageLength = 7,
                           autoWidth = TRUE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#4d4d4d', 'color': '#ffffff'});",
                             "}")))  
})