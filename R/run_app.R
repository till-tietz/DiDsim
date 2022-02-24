#' shinyApp for staggered DiD simulation
#'
#' @name run_app
#'
#' run_app is a wrapper for the ui,server and call elements of the shinyApp
#'
#' @return runs shinyApp
#' @export


library(shiny)

run_app <- function(){

  ui <- navbarPage("DiDsim",
                   navbarMenu("Menu",
                              tabPanel("Simulation",
                                       fluidPage(
                                         sidebarPanel(
                                           width = 4,
                                           selectInput("g_select","Group:",
                                                       choices = list(" ","Group 1","Group 2","Group 3"),
                                                       selected = " ",
                                                       width = "50%"),
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


run_app()
