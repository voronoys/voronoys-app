library(plotly)
library(scales)
library(mapview)

donut_plot <- function(data, value_var, group_var, turno_filtro, colors_pal) {
  data <- as.data.frame(data)
  data <- subset(data, NUM_TURNO == turno_filtro)
  data <- data[order(data[, value_var], decreasing = T), ]
  data[, group_var] <- factor(data[, group_var], levels = data[, group_var])
  
  pos_var <- which(names(data) %in% c(value_var, group_var))
  names(data)[pos_var] <- c("group_var", "value_var")
  
  donut_ply <- plot_ly(data = data, 
                       labels = ~group_var,
                       # color = ~`group_var`,
                       values = ~value_var, 
                       colors = colors_pal
  ) %>%
    add_pie(hole = 0.6) %>%
    layout(title = "",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           showlegend = T)
  
  return(donut_ply)
}

bar_plot <- function(data, value_var, group_var, turno_filtro, colors_pal){
  data <- as.data.frame(data)
  data <- subset(data, NUM_TURNO == turno_filtro)
  data <- data[order(data[, value_var], decreasing = T), ]
  data[, group_var] <- factor(data[, group_var], levels = data[, group_var])
  
  pos_var <- which(names(data) %in% c(value_var, group_var))
  names(data)[pos_var] <- c("group_var", "value_var")
  
  bar <- plot_ly(data,
                 x = ~value_var, 
                 y = ~group_var, 
                 color = ~`group_var`,
                 colors = colors_pal,
                 hoverinfo = "text",
                 text = ~paste0(`group_var`, ": ", percent(value_var/sum(value_var))),
                 type = 'bar', 
                 orientation = 'h')  %>%
    layout(title = "", 
           xaxis = list(title = "Quantidade de votos"),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           margin = list(l = 3, r = 0, t = 25, b = 45),
           showlegend = T)
  bar
}

mapa_uf <- function(data, poly, ano_eleicao, turno_filtro){
  uf_governador <- data %>%
    filter(ANO_ELEICAO == ano_eleicao, NUM_TURNO == turno_filtro) %>%
    group_by(UF, SIGLA_PARTIDO, NOME_URNA_CANDIDATO) %>%
    summarise(QTDE_VOTOS_TOT = sum(QTDE_VOTOS, na.rm = T)) %>%
    # ungroup() %>%
    group_by(UF) %>%
    arrange(desc(QTDE_VOTOS_TOT)) %>%
    # filter(DESC_SIT_TOT_TURNO == 'ELEITO' & ANO_ELEICAO == ano_eleicao) %>%
    mutate(ordem = rank(-QTDE_VOTOS_TOT)) %>%
    filter(ordem == 1) %>%
    summarise(NOME_URNA_CANDIDATO = first(NOME_URNA_CANDIDATO), 
              SIGLA_PARTIDO = first(SIGLA_PARTIDO))
  
  uf_governador_shape <- merge(poly, uf_governador)
  uf_governador_shape$SIGLA_PARTIDO <- as.factor(uf_governador_shape$SIGLA_PARTIDO)
  
  paleta_col <- ggthemes::tableau_color_pal("tableau20")(n_distinct(uf_governador_shape$SIGLA_PARTIDO))
  cores <- paleta_col[as.factor(uf_governador_shape$SIGLA_PARTIDO)]
  
  uf_governador_shape$popup <- paste(uf_governador_shape$SIGLA_PARTIDO, uf_governador_shape$NOME_URNA_CANDIDATO, sep = " - ")
  
  bbox_mun <- bbox(uf_governador_shape)
  map <- leaflet(data = uf_governador_shape) %>%
    addTiles() %>% 
    fitBounds(bbox_mun[1, 1], bbox_mun[2, 1], bbox_mun[1, 2], bbox_mun[2, 2])
  
  mapa_uf <- map %>%
    addPolygons(stroke = T,
                color = "#000000", 
                group = uf_governador_shape$UF,
                popup = uf_governador_shape$popup,
                label = uf_governador_shape$NOME,
                weight = 0.1,
                smoothFactor = 0.1,
                fillOpacity = 0.7,
                fillColor = cores,
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = T)) %>%
    addLegend("bottomright", colors = paleta_col, labels = levels(uf_governador_shape$SIGLA_PARTIDO),
              opacity = 1)
  mapa_uf
}

mapa_mun <- function(data, poly, ano_eleicao, turno_filtro, uf = NULL, poly_uf = regUF) {
  
  if(!("data.table" %in% class(data))) data <- as.data.table(data)
  
  if (!is.null(uf)) {
    gov_eleicao <- data[ANO_ELEICAO == ano_eleicao & UF == uf, ]  
  } else {
    gov_eleicao <- data[ANO_ELEICAO == ano_eleicao, ]
  }
  
  
  mun_turno <- gov_eleicao %>%
    group_by(COD_MUN_IBGE) %>%
    filter(NUM_TURNO == turno_filtro) %>%
    select(COD_MUN_IBGE, NUM_TURNO, NOME_MUNICIPIO, NOME_URNA_CANDIDATO, NOME_CANDIDATO, QTDE_VOTOS) %>%
    unique(.) %>%
    ungroup() %>%
    mutate(COD_MUN_IBGE = as.numeric(as.character(COD_MUN_IBGE)), 
           NOME_URNA_CANDIDATO = as.character(NOME_URNA_CANDIDATO))
  
  mun_turno <- mun_turno %>%
    dcast(COD_MUN_IBGE + NOME_MUNICIPIO + NUM_TURNO~NOME_URNA_CANDIDATO, value.var = "QTDE_VOTOS", fun.aggregate = sum)
  
  candidatos_nome <- names(mun_turno)[-c(1:3)]
  ELEITO_MUN  <- candidatos_nome[apply(mun_turno[, -c(1:3)], 1, which.max)]
  VOTOS_TOTAL <- apply(mun_turno[, -c(1:3)], 1, max, na.rm = T)
  
  mun_turno$ELEITO_MUN <- ELEITO_MUN
  mun_turno$VOTOS_TOTAL <- VOTOS_TOTAL
  
  mun_turno_shape <- poly
  names(mun_turno_shape)[1] <- "COD_MUN_IBGE"
  mun_turno_shape <- merge(mun_turno_shape, mun_turno, all.x = T)
  
  if (!is.null(uf)) {
    mun_turno_shape <- subset(mun_turno_shape, UF == uf)
  }
  
  mun_turno_shape$ELEITO_MUN <- as.factor(mun_turno_shape$ELEITO_MUN)
  paleta_col <- ggthemes::tableau_color_pal("tableau20")(nlevels(mun_turno_shape$ELEITO_MUN))
  cores <- paleta_col[as.factor(mun_turno_shape$ELEITO_MUN)]
  
  mun_turno_shape$popup <- sprintf("<strong>%s</strong><br>Candidato(a): %s<br><br>Votos: %s<br>", 
                                   mun_turno_shape$NOME_MUNICIPIO, 
                                   mun_turno_shape$ELEITO_MUN, 
                                   mun_turno_shape$VOTOS_TOTAL)
  
  bbox_mun <- bbox(mun_turno_shape)
  map <- leaflet(data = mun_turno_shape) %>%
    addTiles() %>% 
    fitBounds(bbox_mun[1, 1], bbox_mun[2, 1], bbox_mun[1, 2], bbox_mun[2, 2])
  
  mapa_eleicao_1 <- map %>%
    addPolygons(data = poly_uf, fillOpacity = 0, weight = 0.75, color = "#222222") %>%
    addPolygons(stroke = T,
                color = "black", 
                group = mun_turno_shape$UF,
                popup = popupTable(mun_turno_shape@data, zcol = names(mun_turno_shape@data[, -c(1:6, ncol(mun_turno_shape) - 1, ncol(mun_turno_shape))])),
                # popup = mun_turno_shape$popup,
                label = paste(mun_turno_shape$NOME_MUNICIPIO, mun_turno_shape$UF, sep = " - "),
                weight = 0.3,
                smoothFactor = 0.1,
                # opacity = 1.0,
                fillOpacity = 0.7,
                fillColor = cores,
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = T)) %>%
    addLegend("bottomright", colors = paleta_col, labels = levels(mun_turno_shape$ELEITO_MUN),
              opacity = 1)
  mapa_eleicao_1
  
}
