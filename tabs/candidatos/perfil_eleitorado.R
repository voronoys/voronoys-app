perfil_eleitorado <- tabPanel(title = "Perfil do eleitorado", 
                              value = "candidatos_perfil_eleitorado",
                              br(), hr(),
                              column(width = 11,
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_ano", 
                                                        label = "Ano", 
                                                        choices = 2014,
                                                        # choices = anos, 
                                                        selected = 2014, 
                                                        options = 2014)
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_cargo", 
                                                        label = "Cargo", 
                                                        choices = "PRESIDENTE",
                                                        # choices = cargos, 
                                                        selected = "PRESIDENTE",
                                                        options = "PRESIDENTE")
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_turno", 
                                                        label = "Turno", 
                                                        choices = c("1º turno", "2º turno"), 
                                                        selected = "1º",
                                                        options = list(`live-search` = TRUE))
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_partido", 
                                                        label = "Partido", 
                                                        choices = levels(factor(x = partidos,
                                                                                levels = partidos)), 
                                                        selected = "PT",
                                                        options = list(`live-search` = TRUE,
                                                                       `none-selected-text` = "Nenhum partido selecionado"))
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_estado", 
                                                        label = "Estado", 
                                                        choices = "SP", 
                                                        selected = "SP",
                                                        options = list(`live-search` = TRUE,
                                                                       `none-selected-text` = "Nenhum estado selecionado"))
                                     ),
                                     column(width = 2,
                                            pickerInput(inputId = "perfil_candidato_voronoi_municipio", 
                                                        label = "Município", 
                                                        choices = municipios, 
                                                        selected = "3550308",
                                                        options = list(`live-search` = TRUE,
                                                                       `none-selected-text` = "Nenhum município selecionado selecionado"))
                                     )
                              ),
                              column(width = 1,
                                     br(), br(), 
                                     actionBttn(inputId = "perfil_candidato_voronoi_gerar_visualizacoes", 
                                                label = "Selecionar", 
                                                style = "fill", 
                                                color = "success", 
                                                icon = icon("check")) 
                              ),
                              column(width = 6,
                                     conditionalPanel(condition = "input.perfil_candidato_voronoi_gerar_visualizacoes > 0",
                                                      br(), hr(), br(),
                                                      HTML("<center><h1>MAPA DOS VOTOS POR SEÇÃO ELEITORAL</h1></center>"),
                                                      br(), br(),
                                                      column(width = 12,
                                                             leafletOutput("mapa_votos_voronoi", height = "500px")
                                                      )
                                     )
                              ),
                              column(width = 6,
                                     conditionalPanel(condition = "input.perfil_candidato_voronoi_gerar_visualizacoes > 0",
                                                      br(), hr(), br(),
                                                      HTML("<center><h1>MAPA DA RENDA PER CAPITA POR S.E.</h1></center>"),
                                                      br(), br(),
                                                      column(width = 12,
                                                             leafletOutput("mapa_renda_voronoi", height = "500px")
                                                      )
                                     )
                              )#,
                              # column(width = 12,
                              #        conditionalPanel(condition = "input.perfil_candidato_voronoi_gerar_visualizacoes > 0",
                              #                         br(), hr(), br(),
                              #                         # HTML("<center><h1>MAPA DA RENDA PER CAPITA POR SEÇÃO ELEITORAL</h1></center>"),
                              #                         br(),
                              #                         column(width = 12,
                              #                                mapviewOutput("mapa_conjunto")
                              #                         )
                              #        )
                              # )
)