partido <- tabPanel(title = "Análise Partidária", 
                    value = "partidos",
                    br(), hr(),
                    column(width = 11,
                           column(width = 2,
                                  pickerInput(inputId = "partido_ano", 
                                              label = "Ano", 
                                              choices = c(2010, 2014), 
                                              selected = 2014, 
                                              options = list(`live-search` = TRUE))
                           ),
                           column(width = 2,
                                  pickerInput(inputId = "partido_cargo", 
                                              label = "Cargo", 
                                              choices = cargos, 
                                              selected = "PRESIDENTE",
                                              options = list(`live-search` = TRUE))
                           ),
                           column(width = 2,
                                  pickerInput(inputId = "partido_partido", 
                                              label = "Partido", 
                                              choices = levels(factor(x = c("Todos os partidos", partidos),
                                                                      levels = c("Todos os partidos", partidos))), 
                                              selected = "Todos os partidos",
                                              options = list(`live-search` = TRUE,
                                                             `none-selected-text` = "Nenhum partido selecionado"))
                           ),
                           column(width = 2,
                                  pickerInput(inputId = "partido_estado", 
                                              label = "Estado", 
                                              choices = levels(factor(x = c("Todos os estados", estados),
                                                                      levels = c("Todos os estados", estados))), 
                                              selected = "Todos os estados",
                                              options = list(`live-search` = TRUE,
                                                             `none-selected-text` = "Nenhum estado selecionado"))
                           )
                    ), 
                    column(width = 1,
                           br(), br(), 
                           actionBttn(inputId = "partidos_gerar_visualizacoes", 
                                      label = "Selecionar", 
                                      style = "fill", 
                                      color = "success", 
                                      icon = icon("check")) 
                    ),
                    column(width = 6,
                           conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                            br(), hr(), br(),
                                            HTML("<center><h1>Doações em campanha por setor</h1></center>"),
                                            column(width = 12,
                                                   plotlyOutput("rank_setor")
                                            )           
                           )
                    ),
                    column(width = 6,
                           conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                            br(), hr(), br(),
                                            HTML("<center><h1>Mapa de Candidatos</h1></center>"),
                                            column(width = 12,
                                                   leafletOutput("mapa_cand")
                                            )           
                           )
                    ),
                    column(width = 6,
                           conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                            br(), hr(), br(),
                                            HTML("<center><h1>Proporção de gênero</h1></center>"),
                                            column(width = 12,
                                                   plotlyOutput("donut_sexo")
                                            )           
                           )
                    ),
                    column(width = 6,
                           conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                            br(), hr(), br(),
                                            HTML("<center><h1>Proporção de raça</h1></center>"),
                                            column(width = 12,
                                                   plotlyOutput("donut_raca")
                                            )           
                           )
                    ),
                    column(width = 6,
                           conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                            br(), hr(), br(),
                                            HTML("<center><h1>Grau de instrução</h1></center>"),
                                            column(width = 12,
                                                   dataTableOutput("tabela")
                                            )           
                           )
                    )
)