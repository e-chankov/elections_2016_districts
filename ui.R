parties.titles <- c("rodina", "communists",
                    "pensioners", "edinaya_rossia",
                    "greens", "civil_platform",
                    "LDPR", "PARNAS",
                    "ROST", "civil_power",
                    "yabloko", "CPRF",
                    "patriots", "sprav_rossia")
names(parties.titles) <- c("Родина", "Коммунисты России",
                           "Партия пенсионеров",
                           "Единая Россия","Зеленые",
                           "Гражданская платформа",
                           "ЛДПР", "ПАРНАС",
                           "Партия роста", "Гражданская сила", 
                           "Яблоко", "КПРФ",
                           "Патриоты России",
                           "Справедливая Россия")

pacman::p_load(shinythemes, leaflet, formattable, DT)

navbarPage(title = 'Выборы-2016', theme = shinytheme('cerulean'),
                 tabPanel('Результаты', 
                          fluidRow(
                            column(2,
                                   selectInput('party', 'Партия', choices = parties.titles),
                                   uiOutput('party_result'),
                                   hr(),
                                   uiOutput('slider_results'),
                                   plotOutput('hist_results', height = 260),
                                   hr(),
                                   uiOutput('sources')
                            ),
                            column(10,
                                   column(9,
                                          leafletOutput('RusMap_results', width = '100%', height = 480),
                                          br()
                                   ),
                                   column(3, leafletOutput('MskMap_results', width = '100%', height = 230),
                                          br(),
                                          leafletOutput('SpbMap_results', width = '100%', height = 230),
                                          br()
                                   ),
                                   fluidRow(
                                     column(6, dataTableOutput('leftTable_results')),
                                     column(6, dataTableOutput('rightTable_results'))
                                   )
                            )
                          )
                 ),
                 
                 tabPanel('Явка',
                          fluidRow(
                            column(2,
                                   uiOutput('total_turnout'),
                                   hr(),
                                   uiOutput('slider_turnout'),
                                   uiOutput('message_turnout'),
                                   br(),
                                   formattableOutput("changesTable", width = "100%")) ,
                            column(10,
                                   column(9,
                                          leafletOutput('RusMap_turnout', width = '100%', height = 480),
                                          br()
                                   ),
                                   column(3, leafletOutput('MskMap_turnout', width = '100%', height = 230),
                                          br(),
                                          leafletOutput('SpbMap_turnout', width = '100%', height = 230),
                                          br()
                                   ),
                                   fluidRow(
                                     column(6, dataTableOutput('leftTable_turnout')),
                                     column(6, dataTableOutput('rightTable_turnout'))
                                   )
                            )
                          )
                 ),
                 
                 tabPanel('Сравнения',
                          fluidRow(
                            column(2,
                                   selectInput("party1", "Партия 1", choices = parties.titles, selected = "CPRF"),
                                   selectInput("party2", "Партия 2", choices = parties.titles, selected = "LDPR"),
                                   br(),
                                   uiOutput('message_diff'),
                                   br(),
                                   plotOutput('hist_diff', height = 260)
                            ),
                            column(10,
                                   column(9,
                                          leafletOutput('RusMap_diff', width = '100%', height = 480),
                                          br()
                                   ),
                                   column(3, leafletOutput('MskMap_diff', width = '100%', height = 230),
                                          br(),
                                          leafletOutput('SpbMap_diff', width = '100%', height = 230),
                                          br()
                                   ),
                                   fluidRow(
                                     column(6, dataTableOutput('leftTable_diff')),
                                     column(6, dataTableOutput('rightTable_diff'))
                                   )
                            )
                          )
                 )
)

