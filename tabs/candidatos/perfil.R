perfil <- tabPanel(title = "Perfil do candidato", 
                   value = "candidatos_perfil",
                   br(), hr(),
                   column(width = 11,
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_ano", 
                                             label = "Ano", 
                                             choices = anos, 
                                             selected = 2014, 
                                             options = list(`live-search` = TRUE))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_cargo", 
                                             label = "Cargo", 
                                             choices = cargos, 
                                             selected = "PRESIDENTE",
                                             options = list(`live-search` = TRUE))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_turno", 
                                             label = "Turno", 
                                             choices = c("1º turno", "2º turno"), 
                                             selected = "1º",
                                             options = list(`live-search` = TRUE))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_partido", 
                                             label = "Partido", 
                                             choices = levels(factor(x = c("Todos os partidos", partidos),
                                                                     levels = c("Todos os partidos", partidos))), 
                                             selected = "Todos os partidos",
                                             options = list(`live-search` = TRUE,
                                                            `none-selected-text` = "Nenhum partido selecionado"))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_estado", 
                                             label = "Estado", 
                                             choices = levels(factor(x = c("Todos os estados", estados),
                                                                     levels = c("Todos os estados", estados))), 
                                             selected = "Todos os estados",
                                             options = list(`live-search` = TRUE,
                                                            `none-selected-text` = "Nenhum estado selecionado"))
                          ),
                          column(width = 2,
                                 pickerInput(inputId = "perfil_candidato_cpf", 
                                             label = "Candidato", 
                                             choices = NULL, 
                                             selected = NULL,
                                             options = list(`live-search` = TRUE,
                                                            `none-selected-text` = "Nenhum candidato selecionado selecionado"))
                          )
                   ), 
                   column(width = 1,
                          br(), br(), 
                          actionBttn(inputId = "perfil_candidato_gerar_visualizacoes", 
                                     label = "Selecionar", 
                                     style = "fill", 
                                     color = "success", 
                                            icon = icon("check")) 
                   ),
                   column(width = 6,
                          conditionalPanel(condition = "input.perfil_candidato_gerar_visualizacoes > 0",
                                           br(), hr(), br(),
                                           HTML("<center><h1>MAPA DA PROPORÇÃO DE VOTOS</h1></center>"),
                                           br(), 
                                           column(width = 12,
                                                  leafletOutput("perfil_candidatos_mapa", height = "500px")
                                           )
                          )
                   ),
                   column(width = 6,
                          conditionalPanel(condition = "input.perfil_candidato_gerar_visualizacoes > 0",
                                           br(), hr(), br(),
                                           HTML("<center><h1>PERFIL DO CANDIDATO</h1></center>"),
                                           
                                           column(width = 12,
                                                  uiOutput("perfil_candidato") 
                                           )           
                          )
                   )
)