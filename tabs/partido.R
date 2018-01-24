tab_files <- list.files(path = "tabs/partidos", full.names = T)
suppressMessages(lapply(tab_files, source))

partido <- tabPanel(title = "Partidos", 
                     value = "partidos",
                     tabsetPanel(
                       partido_geral,
                       partido
                     )
)