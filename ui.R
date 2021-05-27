shinyUI(
  navbarPage(
    "COVID-19 Data Visualizer",
    tags$style(type = 'text/css', '.navbar {#font-family: Arial; font-size: 20px; }'), 
    selected = "Global Status",
    collapsible = TRUE,
    inverse = TRUE,
    theme = shinytheme("superhero"),
    tabPanel(
      "Global Status",
      width = "100%",
      div(plotlyOutput("figG"), align = "center"),
      br(),
      div(plotlyOutput("global_time"), align = "center"),
      br(),
      div(plotlyOutput("global_cfr"), align = "center"),
      br(),
      div(plotlyOutput("global_p"), align = "center"),
      br(),
      div(plotlyOutput("global_recov_dead"), align = "center")
    ),
    tabPanel("Analytics",
             fluidPage(
               sidebarPanel(
                 width = 2,
                 selectInput(
                   "countries",
                   label = "Select Countries",
                   choices =
                     country_list,
                   selected = c(country_list[which(country_list == "Malaysia")], country_list[which(country_list ==
                                                                                                      "Nepal")], country_list[which(country_list == "Pakistan")]),
                   multiple = T
                 ),
                 selectInput(
                   "Scale",
                   label = "Comparative Charts' Scale",
                   choices =
                     c("Linear", "Log"),
                   selected = "Linear",
                   multiple = F
                 ),
                 submitButton(text = "View")
               ),
               mainPanel (h1(""),
                          tabsetPanel(
                            tabPanel(
                              "Comparative Charts",
                              div(plotlyOutput("fig_confirm"), align = "center"),
                              br(),
                              div(plotlyOutput("fig_dead"), align = "center"),
                              br(),
                              div(plotlyOutput("fig_recov"), align = "center")
                            ),
                            tabPanel(
                              "Metrics",
                              div(plotlyOutput("fig_confirm_S"), align = "center"),
                              br(),
                              div(plotlyOutput("fig_confirm_D"), align = "center"),
                              br(),
                              div(plotlyOutput("fig_Ratio"), align = "center"),
                              br(),
                              div(plotlyOutput("fig_cfr_print"), align = "center")
                            ),
                            tabPanel("Growth",
                                     div(plotlyOutput("fig_cp_print"), align = "center"))
                          ))
             ))
  )
)