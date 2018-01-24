##-- + Atualizações dos filtros ----

##-- ++ Atualizações dos turnos ----
observeEvent(c(input$perfil_candidato_voronoi_ano, 
               input$perfil_candidato_voronoi_cargo),{
                 
                 ano <- isolate(input$perfil_candidato_voronoi_ano)
                 cargo <- isolate(input$perfil_candidato_voronoi_cargo)
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo) %>%
                     collect()
                   
                   ##-- Setando o cargo default
                   turnos <- unique(chaves_sub$NUM_TURNO)
                   turno_default <- input$perfil_candidato_voronoi_turno
                   
                   if(!(turno_default %in% turnos)){
                     turno_default <- "1º turno"
                   }
                   
                   turnos <- paste0(turnos, "º turno")
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(sessio = session,
                                     inputId = "perfil_candidato_voronoi_turno", 
                                     label = "Turno", 
                                     choices = turnos, 
                                     selected = turno_default)
                 }
                 
               }, priority = 2)
##-- ++ Atualizações dos partidos ----
observeEvent(c(input$perfil_candidato_voronoi_ano, 
               input$perfil_candidato_voronoi_cargo, 
               input$perfil_candidato_voronoi_turno),{
                 
                 ano <- isolate(input$perfil_candidato_voronoi_ano)
                 cargo <- isolate(input$perfil_candidato_voronoi_cargo)
                 turno <- isolate(input$perfil_candidato_voronoi_turno)
                 turno <- ifelse(turno == "1º turno", "1", "2")
                 
                 if(!is.null(cargo)){
                   chaves_sub <- chaves %>%
                     filter(ANO_ELEICAO == ano & DESCRICAO_CARGO == cargo & NUM_TURNO == turno) %>%
                     collect()
                   
                   ##-- Setando o cargo default
                   partidos <- levels(factor(x = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO))),
                                             levels = c("Todos os partidos", sort(unique(chaves_sub$SIGLA_PARTIDO)))))
                   partido_default <- input$perfil_candidato_voronoi_partido
                   
                   if(!(partido_default %in% partidos)){
                     partido_default <- "Todos os partidos"
                   }
                   
                   ##-- Atualizando os partidos ----
                   updatePickerInput(session = session,
                                     inputId = "perfil_candidato_voronoi_partido",
                                     label = "Partido", 
                                     choices = partidos, 
                                     selected = partido_default)  
                 }
                 
               }, priority = 3)

##-- ++ Atualizações dos candidatos ----
observeEvent(c(input$perfil_candidato_voronoi_ano, 
               input$perfil_candidato_voronoi_cargo, 
               input$perfil_candidato_voronoi_turno, 
               input$perfil_candidato_voronoi_partido,
               input$perfil_candidato_voronoi_estado),{
                 
                 ano <- isolate(input$perfil_candidato_voronoi_ano)
                 cargo <- isolate(input$perfil_candidato_voronoi_cargo)
                 turno <- isolate(input$perfil_candidato_voronoi_turno)
                 turno <- ifelse(turno == "1º turno", "1", "2")
                 partido <- isolate(input$perfil_candidato_voronoi_partido)
                 estado <- isolate(input$perfil_candidato_voronoi_estado)
                 
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
                                         inputId = "perfil_candidato_voronoi_cpf", 
                                         label = "Candidato", 
                                         choices = candidatos, 
                                         selected = NULL)
                     }
                   }
                 }
                 
               }, priority = 5)

base_votos <- reactive({
  
  COD_MUN <- input$perfil_candidato_voronoi_municipio
  PARTIDO <- input$perfil_candidato_voronoi_partido
  TURNO <- input$perfil_candidato_voronoi_turno
  TURNO <- ifelse(TURNO == "1º turno", "1", "2")
  
  votos_aux <- votos %>% filter(NUM_TURNO == TURNO)
  
  if(COD_MUN != "TODOS MUNICIPIOS"){
    votos_aux <- votos_aux %>% 
      filter(CODIGO_MUNICIPIO == COD_MUN) %>% 
      collect() %>%
      reshape2::dcast(key_merge ~ SIGLA_PARTIDO, 
                      value.var = "QTDE_VOTOS", fun.aggregate = sum) 
    
    chaves_merge <- unique(votos_aux$key_merge)
    
    enderecos_aux <- enderecos %>%
      filter(key_merge %in% chaves_merge) %>% 
      collect()
    
  } else{
    votos_aux <- votos_aux %>% 
      collect() %>%
      reshape2::dcast(key_merge ~ SIGLA_PARTIDO, 
                      value.var = "QTDE_VOTOS", fun.aggregate = sum)
    
    enderecos_aux <- enderecos %>%
      collect()
  }
  
  if(any(names(votos_aux) == "NA")) names(votos_aux)[names(votos_aux) == "NA"] <- "BRANCOS/NULOS"
  
  votos_aux$tot <- apply(votos_aux[,2:ncol(votos_aux)], 1, sum, na.rm = T)
  col_aux <- ncol(votos_aux)
  votos_aux <- inner_join(votos_aux, enderecos_aux, by = "key_merge")
  
  votos_aux <- votos_aux %>% 
    group_by(ID) %>%
    summarise_at(.vars = 2:col_aux, .funs = sum) %>% 
    mutate_at(.vars = 2:(col_aux - 1), .funs = funs(./tot))
  
  sf_secoes <- inner_join(lat_long, votos_aux, by = "ID") %>% st_transform(4326)
  
  paleta1 <- colorNumeric(palette = colorRamp(RColorBrewer::brewer.pal(9, "Reds"), 
                                              interpolate = "linear"), 
                          # domain = range(sf_secoes[PARTIDO][[1]], na.rm = T), reverse = F)
                          domain = c(0, 1), reverse = F)
  
  aux2 <- as.data.frame(sf_secoes) %>% select(PARTIDO)
  aux2 <- aux2[PARTIDO] %>% unclass()
  aux2 <- aux2[[1]] %>% `*`(100) %>% 
    round(2) 
  
  return(list(percentuais = aux2, sf_secoes = sf_secoes, paleta = paleta1))
})

base_renda <- reactive({
  
  COD_MUN <- input$perfil_candidato_voronoi_municipio
  
  if(COD_MUN != "TODOS MUNICIPIOS"){
    votos_aux <- votos %>% 
      filter(CODIGO_MUNICIPIO == COD_MUN) %>% 
      collect() %>%
      reshape2::dcast(key_merge ~ SIGLA_PARTIDO, 
                      value.var = "QTDE_VOTOS", fun.aggregate = sum) 
    
    chaves_merge <- unique(votos_aux$key_merge)
    
    enderecos_aux <- enderecos %>%
      filter(key_merge %in% chaves_merge) %>% 
      collect()
    
  } else{
    votos_aux <- votos %>% 
      collect() %>%
      reshape2::dcast(key_merge ~ SIGLA_PARTIDO, 
                      value.var = "QTDE_VOTOS", fun.aggregate = sum)
    
    enderecos_aux <- enderecos %>%
      collect()
  }
  
  col_aux <- ncol(votos_aux)
  
  votos_aux <- inner_join(votos_aux, enderecos_aux, by = "key_merge")
  
  votos_aux <- votos_aux %>% 
    group_by(ID) %>%
    summarise_at(.vars = 2:col_aux, .funs = sum)
  
  sf_secoes <- inner_join(lat_long, votos_aux, by = "ID") %>% st_transform(4326)
  
  paleta1 <- colorNumeric(palette = colorRamp(RColorBrewer::brewer.pal(5, "RdBu")), 
                          domain = range(sf_secoes$renda, na.rm = T), reverse = TRUE)
  
  return(list(sf_secoes = sf_secoes, paleta = paleta1))
})

output$mapa_votos_voronoi <- renderLeaflet({
  
  PARTIDO <- input$perfil_candidato_voronoi_partido
  
  base_votos_df <- base_votos()
  aux2 <- base_votos_df$percentuais
  sf_secoes <- base_votos_df$sf_secoes
  paleta1 <- base_votos_df$paleta
  
  labels <- sprintf(
    "<strong>%s</strong> %g </br> <strong>%s</strong> %g",
    "% Votos", aux2, 
    "Renda", sf_secoes$renda
  ) %>% lapply(htmltools::HTML)
  
  leaflet(data = sf_secoes) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addPolygons(color = "#222222", weight = 0.1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.7,
                fillColor = ~paleta1(get(PARTIDO)),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto")) %>% 
    addLegend("bottomright", pal = paleta1, values = ~get(PARTIDO),
              opacity = .9, 
              title = "Votos", 
              labFormat = labelFormat(suffix= "%", digits = 4, transform = function(x) x*100))
})

output$mapa_renda_voronoi <- renderLeaflet({
  
  base_renda_list <- base_renda()
  sf_secoes <- base_renda_list$sf_secoes
  paleta1 <- base_renda_list$paleta
  
  labels <- sprintf(
    "<strong>%s</strong></br> %g",
    "Renda per capita", sf_secoes$renda
  ) %>% lapply(htmltools::HTML)
  
  brks <- c(0,  255,  765, 1912, 3825)
  
  lbl <- LETTERS[5:1]
  
  teste <- findInterval(x = sf_secoes$renda, 
                        vec = brks)
  
  sf_secoes$classe <- factor(teste, levels = 1:5, labels = LETTERS[5:1], ordered = T)
  
  paleta1 <- colorFactor(palette = RColorBrewer::brewer.pal(5, 'Reds'), sf_secoes$classe) 
  
  leaflet(data = sf_secoes) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addPolygons(color = "#222222", weight = 0.1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.7,
                fillColor = ~paleta1(classe),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto")) %>% 
    addLegend("bottomright", pal = paleta1, values = ~classe,
              title = "Classe - segundo r.p.c.",
              opacity = .9)
})


# output$mapa_conjunto <- renderMapview({
#   browser()
#   PARTIDO <- input$perfil_candidato_voronoi_partido
# 
#   base_votos_df <- base_votos()
#   aux2 <- base_votos_df$percentuais
#   sf_secoes <- base_votos_df$sf_secoes
#   paleta1 <- base_votos_df$paleta
# 
#   labels <- sprintf(
#     "<strong>%s</strong> %g </br> <strong>%s</strong> %g",
#     "% Votos", aux2,
#     "Renda", sf_secoes$renda
#   ) %>% lapply(htmltools::HTML)
# 
#   mapa_votos_voronoi2 <- leaflet(data = sf_secoes) %>%
#     addProviderTiles(providers$CartoDB.Positron) %>%
#     addPolygons(color = "#222222", weight = 0.1, smoothFactor = 0.5,
#                 opacity = 1.0, fillOpacity = 0.7,
#                 fillColor = ~paleta1(get(PARTIDO)),
#                 highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                     bringToFront = TRUE),
#                 label = labels,
#                 labelOptions = labelOptions(
#                   style = list("font-weight" = "normal", padding = "3px 8px"),
#                   textsize = "12px",
#                   direction = "auto"))
# 
#   base_votos_df <- base_votos()
#   aux2 <- base_votos_df$percentuais
#   sf_secoes <- base_votos_df$sf_secoes
#   paleta1 <- base_votos_df$paleta
# 
#   labels <- sprintf(
#     "<strong>%s</strong> %g </br> <strong>%s</strong> %g",
#     "% Votos", aux2,
#     "Renda", sf_secoes$renda
#   ) %>% lapply(htmltools::HTML)
# 
#   mapa_renda_voronoi2 <- leaflet(data = sf_secoes) %>%
#     addProviderTiles(providers$CartoDB.Positron) %>%
#     addPolygons(color = "#222222", weight = 0.1, smoothFactor = 0.5,
#                 opacity = 1.0, fillOpacity = 0.7,
#                 fillColor = ~paleta1(get(PARTIDO)),
#                 highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                     bringToFront = TRUE),
#                 label = labels,
#                 labelOptions = labelOptions(
#                   style = list("font-weight" = "normal", padding = "3px 8px"),
#                   textsize = "12px",
#                   direction = "auto"))
# 
#   latticeView(mapa_votos_voronoi2, mapa_renda_voronoi2, ncol = 2, sync = list(c(1, 2)), sync.cursor = T)
#   
# })