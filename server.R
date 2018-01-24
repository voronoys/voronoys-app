shinyServer(function(input, output, session){
  ##-- HOME ----
  source("tabs/server.home.R", local = TRUE)
  ##-- ELEIÇÕES ----
  source("tabs/server.eleicoes.R", local = TRUE)
  ##-- PARTIDOS ----
  source("tabs/server.partidos.geral.R", local = TRUE)
  source("tabs/server.partidos.R", local = TRUE)
  ##-- CANDIDATOS ----
  source("tabs/server.candidatos.R", local = TRUE)
  source("tabs/server.candidatos_voronoi.R", local = TRUE)
  ##-- SOBRE ----
  source("tabs/server.sobre.R", local = TRUE)
})