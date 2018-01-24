eleicoes <- tabPanel(title = "Eleições", 
                     value = "eleicoes",
                     br(), hr(),
                     column(width = 11,
                            column(width = 2,
                                   pickerInput(inputId = "eleicoes_ano", 
                                               label = "Ano", 
                                               choices = anos, 
                                               selected = 2014, 
                                               options = list(`live-search` = TRUE))
                            ),
                            column(width = 2,
                                   pickerInput(inputId = "eleicoes_cargo", 
                                               label = "Cargo", 
                                               choices = cargos, 
                                               selected = "PRESIDENTE",
                                               options = list(`live-search` = TRUE))
                            ),
                            column(width = 2,
                                   pickerInput(inputId = "eleicoes_turno", 
                                               label = "Turno", 
                                               choices = c("1º turno", "2º turno"), 
                                               selected = "1º",
                                               options = list(`live-search` = TRUE))
                            ),
                            column(width = 2,
                                   pickerInput(inputId = "eleicoes_estado", 
                                               label = "Estado", 
                                               choices = levels(factor(x = c("Todos os estados", estados),
                                                                       levels = c("Todos os estados", estados))), 
                                               selected = "Todos os estados",
                                               options = list(`live-search` = TRUE,
                                                              `none-selected-text` = "Nenhum estado selecionado"))
                            )
                     ), 
                     column(width = 1,
                            actionBttn(inputId = "eleicoes_gerar_visualizacoes", 
                                       label = "Selecionar", 
                                       style = "fill", 
                                       color = "success", 
                                       icon = icon("check")) 
                     ),
                     column(width = 4,
                            br(), hr(), br(),
                            conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes > 0",
                                             HTML("<center><h1>MAPA DAS ELEIÇÕES POR ESTADO</h1></center>"),
                                             br(),
                                             leafletOutput("mapa_uf_geral")
                            )
                     ),
                     column(width = 4,
                            br(), hr(), br(),
                            conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes > 0",
                                             HTML("<center><h1>MAPA DAS ELEIÇÕES POR MUNICÍPIO</h1></center>"),
                                             br(),
                                             leafletOutput("mapa_mun_geral")

                            )
                     ),
                     column(width = 4,
                            br(), hr(), br(),
                            conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes > 0",
                                             HTML("<center><h1>GRÁFICO DE BARRAS DO % DE VOTOS</h1></center>"),
                                             br(),
                                             plotlyOutput("barras_geral")
                                             
                            )
                     )
                     # column(width = 3,
                     #        br(), hr(), br(),
                     #        conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes > 0",
                     #                         HTML("<center><h1>DONUT DA PROPORÇÃO DE VOTOS</h1></center>"),
                     #                         br(),
                     #                         plotlyOutput("donut_geral")
                     #                         
                     #        )
                     # )
)