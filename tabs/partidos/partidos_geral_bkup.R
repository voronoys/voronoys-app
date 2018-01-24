partido_geral <- tabPanel(title = "Análise geral", 
                          value = "partidos_geral",
                          br(), hr(),
                          column(width = 11,
                                 column(width = 2,
                                        pickerInput(inputId = "partido_geral_ano", 
                                                    label = "Ano", 
                                                    choices = anos, 
                                                    selected = 2014, 
                                                    options = list(`live-search` = TRUE))
                                 ),
                                 column(width = 2,
                                        pickerInput(inputId = "partido_geral_cargo", 
                                                    label = "Cargo", 
                                                    choices = cargos, 
                                                    selected = "PRESIDENTE",
                                                    options = list(`live-search` = TRUE))
                                 ),
                                 column(width = 2,
                                        pickerInput(inputId = "partido_geral_partido", 
                                                    label = "Partido", 
                                                    choices = levels(factor(x = c("Todos os partidos", partidos),
                                                                            levels = c("Todos os partidos", partidos))), 
                                                    selected = "Todos os partidos",
                                                    options = list(`live-search` = TRUE,
                                                                   `none-selected-text` = "Nenhum partido selecionado"))
                                 ),
                                 column(width = 2,
                                        pickerInput(inputId = "partido_geral_estado", 
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
                                 actionBttn(inputId = "partidos_gerar_visualizacoes2", 
                                            label = "Selecionar", 
                                            style = "fill", 
                                            color = "success", 
                                            icon = icon("check")) 
                          ),
                          column(width = 6,
                                 conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                                                  br(), hr(), br(),
                                                  HTML("<center><h1>10 MAIORES SETORES DOS DOADORES</h1></center>"),
                                                  column(width = 12,
                                                         highchartOutput("treemap_desp") 
                                                  )           
                                 )
                          ),
                          column(width = 6,
                                 conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                                                  br(), hr(), br(),
                                                  HTML("<center><h1>TREEMAP DAS DOAÇÕES POR PARTIDO</h1></center>"),
                                                  column(width = 12,
                                                         highchartOutput("treemap_doa") 
                                                  )           
                                 )
                          ),
                          column(width = 6,
                                 conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                                                  br(), hr(), br(),
                                                  HTML("<center><h1>MAPA DOS PARTIDOS VENCEDORES</h1></center>"),
                                                  column(width = 12,
                                                         leafletOutput("mapa_partidos_cid") 
                                                  )           
                                 )
                          ),
                          column(width = 6,
                                 conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                                                  br(), hr(), br(),
                                                  HTML("<center><h1>HEATMAP DAS COLIGAÇÕES</h1></center>"),
                                                  column(width = 12,
                                                         plotlyOutput("heatmap_coligacoes") 
                                                  )           
                                 )
                          )
)