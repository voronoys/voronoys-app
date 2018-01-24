tab_files <- list.files(path = "tabs/candidatos", full.names = T)
suppressMessages(lapply(tab_files, source))

candidato <- tabPanel(title = "Candidatos", 
                      value = "candidatos",
                      tabsetPanel(
                        perfil,
                        perfil_eleitorado
                      )
)