#' shinyApp for staggered DiD simulation
#'
#' @name run_app
#'
#' run_app is a wrapper for the ui,server and call elements of the shinyApp
#'
#' @return runs shinyApp
#' @export


library(shiny)
library(shinyWidgets)

run_app <- function(){

  ui <- navbarPage("DiDsim",
                   navbarMenu("Menu",
  # simulation panel        ----------------------------------------

                              tabPanel("Simulation",
                                       fluidPage(
      # data input          ----------------------------------------
                                         sidebarPanel(
                                           width = 4,
                                           sliderInput("panel_length","Panel Length:", value = 3, min = 3, max = 10, step = 1, sep = ""),
                                           selectInput("g_select","Groups:",
                                                       choices = list(" ","Group 1","Group 2","Group 3"),
                                                       selected = " ",
                                                       width = "75%"),
                                           conditionalPanel(
                                             condition = "input.g_select == 'Group 1'",
                                             sliderInput("te1", "Treatment Effect:", 2, min = -10, max = 10),
                                             sliderInput("te_m1", "Treatment Effect Slope:", value = 0.4, min = -1, max = 1, step = 0.05),
                                             sliderInput("g1", "Treatment Time:", value = 1, min = 1, max = 10, step = 1, sep = "")
                                           ),
                                           conditionalPanel(
                                             condition = "input.g_select == 'Group 2'",
                                             sliderInput("te2", "Treatment Effect:", 2, min = -10, max = 10),
                                             sliderInput("te_m2", "Treatment Effect Slope:", value = 0.4, min = -1, max = 1, step = 0.05),
                                             sliderInput("g2", "Treatment Time:", value = 2, min = 1, max = 10, step = 1, sep = "")
                                           ),
                                           conditionalPanel(
                                             condition = "input.g_select == 'Group 3'",
                                             shinyWidgets::materialSwitch(
                                               inputId = "is_treated3",
                                               label = "Treated",
                                               value = FALSE,
                                               status = "primary"
                                             ),
                                             conditionalPanel(
                                               condition = "input.is_treated3 == true",
                                               sliderInput("te3", "Treatment Effect:", 2, min = -10, max = 10),
                                               sliderInput("te_m3", "Treatment Effect Slope:", value = 0.4, min = -1, max = 1, step = 0.05),
                                               sliderInput("g3", "Treatment Time:", value = 3, min = 1, max = 10, step = 1, sep = "")
                                             )
                                           ),
                                           selectInput("presets","Heterogeneity Presets:",
                                                       choices = list("None",
                                                                      "Homogeneous Effects",
                                                                      "Heterogeneity in Levels",
                                                                      "Heterogeneity in Levels (w/ Slopes)",
                                                                      "Heterogeneity in Slopes",
                                                                      "Heterogeneity in Levels and Slopes"),
                                                       selected = "None",
                                                       width = "75%"),
                                           actionButton("run", "Analyze")
                                         ),
      # simulation output   ----------------------------------------
                                         mainPanel(
                                           width = 8,
                                           fluidRow(
                                             plotOutput("sim_did_plot")
                                           ),
                                           fluidRow(
                                             column(4,
                                                    align = "center",
                                                    tableOutput("data_params")
                                             ),
                                             column(4,
                                                    align = "center",
                                                    tableOutput("sim_results")
                                             )
                                           )
                                         )
                                       )
                              )
                   )
  )

  server <- function(input, output, session){

  }

  shiny::shinyApp(ui, server)
}



