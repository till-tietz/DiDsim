#' shinyApp for staggered DiD simulation
#'
#' @return None (invisible NULL)
#' @export
#'
#' @import shiny
#' @import shinyWidgets
#' @import tibble
#' @import dplyr
#' @import magrittr
#' @import fixest
#' @import ggplot2
#' @import ggplotify
#' @import ggpubr
#' @import did
#' @import tidyr
#' @import PanelMatch
#' @import magrittr

run_app <- function(){

  ui <- navbarPage("DiDsim",
                   navbarMenu("Menu",
                              # simulation panel        ----------------------------------------

                              tabPanel("Simulation",
                                       fluidPage(
                                         # data input          ----------------------------------------
                                         sidebarPanel(
                                           width = 4,
                                           sliderInput("panel_length","Panel Length:", value = 2, min = 2, max = 10, step = 1, sep = ""),
                                           selectInput("g_select","Groups:",
                                                       choices = list(" ","Group 1","Group 2","Group 3"),
                                                       selected = " ",
                                                       width = "75%"),
                                           conditionalPanel(
                                             condition = "input.g_select == 'Group 1'",
                                             sliderInput("te1", "Treatment Effect:", 2, min = -10, max = 10),
                                             sliderInput("te_m1", "Treatment Effect Slope:", value = 0.4, min = -1, max = 1, step = 0.05),
                                             uiOutput("g1")
                                           ),
                                           conditionalPanel(
                                             condition = "input.g_select == 'Group 2'",
                                             sliderInput("te2", "Treatment Effect:", 2, min = -10, max = 10),
                                             sliderInput("te_m2", "Treatment Effect Slope:", value = 0.4, min = -1, max = 1, step = 0.05),
                                             uiOutput("g2")
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
                                               uiOutput("g3")
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
                                                    offset = 2,
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

    # update treatment year slider selector-------------------------------------------------
    observeEvent(input$panel_length, {

      output$g1 <- renderUI({
        sliderInput("g1", "Treatment Time:", value = 1, min = 0, max = input$panel_length[1], step = 1, sep = "")
      })
      output$g2 <- renderUI({
        sliderInput("g2", "Treatment Time:", value = 2, min = 0, max = input$panel_length[1], step = 1, sep = "")
      })
      output$g3 <- renderUI({
        sliderInput("g3", "Treatment Time:", value = 3, min = 0, max = input$panel_length[1], step = 1, sep = "")
      })

    })

    # update parameters based on heterogeneity presets---------------------------------------
    observeEvent(input$presets, {
      if(input$presets == "None") {
        updateSliderInput(inputId = "te1", value = 2)
        updateSliderInput(inputId = "te2", value = 2)
        updateSliderInput(inputId = "te_m1", value = 0.4)
        updateSliderInput(inputId = "te_m2", value = 0.4)
      }
      else if(input$presets == "Homogeneous Effects") {
        updateSliderInput(inputId = "te1", value = input$te1)
        updateSliderInput(inputId = "te2", value = input$te1)
        updateSliderInput(inputId = "te_m1", value = 0)
        updateSliderInput(inputId = "te_m2", value = 0)
      }
      else if(input$presets == "Heterogeneity in Levels") {
        updateSliderInput(inputId = "te1", value = 2)
        updateSliderInput(inputId = "te2", value = input$te1 * 2)
        updateSliderInput(inputId = "te_m1", value = 0)
        updateSliderInput(inputId = "te_m2", value = 0)
      }
      else if(input$presets == "Heterogeneity in Levels (w/ Slopes)") {
        updateSliderInput(inputId = "te1", value = 2)
        updateSliderInput(inputId = "te2", value = input$te1 * 2)
        updateSliderInput(inputId = "te_m1", value = 0.1)
        updateSliderInput(inputId = "te_m2", value = 0.1)
      }
      else if(input$presets == "Heterogeneity in Slopes") {
        updateSliderInput(inputId = "te1", value = input$te1)
        updateSliderInput(inputId = "te2", value = input$te1)
        updateSliderInput(inputId = "te_m1", value = 0.05)
        updateSliderInput(inputId = "te_m2", value = 0.2)
      }
      else if(input$presets == "Heterogeneity in Levels and Slopes") {
        updateSliderInput(inputId = "te1", value = 2)
        updateSliderInput(inputId = "te2", value = input$te1 * 2)
        updateSliderInput(inputId = "te_m1", value = 0.05)
        updateSliderInput(inputId = "te_m2", value = 0.2)
      }
    })

    # generate data ------------------------------------------------------------------------
    gen_dat <- eventReactive(input$run,{

      set.seed(123)

      if(input$is_treated3){
        g3 <- input$g3
        te3 <- input$te3
        te_m3 <- input$te_m3
      }else{
        g3 <- 1000L
        te3 <- 0
        te_m3 <- 0
      }

      tibble::tibble(unit = 1:1000)%>%
        dplyr::mutate(
          unit_fe = rnorm(dplyr::n(), 0, 0.5),
          group = runif(dplyr::n()),
          group = dplyr::case_when(
            group < 0.33 ~ "Group 1",
            group < 0.66 ~ "Group 2",
            TRUE ~ "Group 3"
          ),
          g = dplyr::case_when(
            group == "Group 1" ~ input$g1,
            group == "Group 2" ~ input$g2,
            group == "Group 3" ~ g3,
          )
        ) %>%
        tidyr::expand_grid(year = 0:input$panel_length[1])%>%
        # Year FE
        dplyr::group_by(year)%>%
        dplyr::mutate(year_fe = rnorm(length(year), 0, 1))%>%
        dplyr::ungroup()%>%
        dplyr::mutate(
          treat = (year >= g) & (g %in% 0:input$panel_length[1]),
          rel_year = dplyr::if_else(g == 10000, Inf, as.numeric(year - g)),
          rel_year_binned = dplyr::case_when(
            rel_year == Inf ~ Inf,
            rel_year <= -6 ~ -6,
            rel_year >= 6 ~ 6,
            TRUE ~ rel_year
          ),
          error = rnorm(dplyr::n(), 0, 1),
          # Level Effect
          te =
            (group == "Group 1") * input$te1 * (year >= input$g1) +
            (group == "Group 2") * input$te2 * (year >= input$g2) +
            (group == "Group 3") * te3       * (year >= g3),
          # dynamic Effect
          te_dynamic =
            (group == "Group 1") * (year >= input$g1) * input$te_m1 * (year - input$g1) +
            (group == "Group 2") * (year >= input$g2) * input$te_m2 * (year - input$g2) +
            (group == "Group 3") * (year >= g3)       * te_m3       * (year - g3),
          y = unit_fe + year_fe + te + te_dynamic + error
        )%>%
        as.data.frame()%>%
        dplyr::mutate_at("treat", as.numeric)%>%
        dplyr::mutate(group_CSA = g)%>%
        dplyr::mutate(group_CSA = replace(group_CSA,group_CSA == 10000,0))%>%
        dplyr::mutate(treat_singular = ifelse(g == year,1,0))

    })

    # generate plots -----------------------------------------------------------------------
    output$sim_did_plot <- renderPlot({

      dat <- gen_dat()

      did_plot_out <- did_plot(data = gen_dat(), year = "year", y = "y", group = "group")
      did_panel_out <- did_panel_plot(data = gen_dat(), unit = "unit", year = "year",
                                      group = "group", treat = "treat")

      #legend_1 <- get_legend(did_plot_out)
      #legend_2 <- get_legend(did_panel_out)

      ggpubr::ggarrange(did_panel_out, did_plot_out,
                        ncol = 2, nrow = 1)
    })

    # estimate -----------------------------------------------------------------------------
    output$sim_results <- renderTable({

      id <- showNotification("Computing DiD estimates...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)

      dat <- gen_dat()

      twfe <- fixest::feols(y ~ treat | unit + year, data = dat)
      twfe_att <- unname(twfe[["coefficients"]])
      twfe_se <- unname(twfe[["se"]][1])

      csa <- did::att_gt(yname= 'y',
                         tname= 'year',
                         idname = 'unit',
                         gname = 'group_CSA',
                         est_method = 'reg',
                         control_group = 'nevertreated',
                         base_period = "universal",
                         data = dat)

      csa <- did::aggte(csa, type = "simple")
      csa_att <- csa[["overall.att"]]
      csa_se <- csa[["overall.se"]]

      pm <- PanelMatch::PanelMatch(lag = 1, time.id = "year", unit.id = "unit",
                                   treatment = "treat_singular", refinement.method = "none",
                                   data = as.data.frame(dat), match.missing = TRUE,
                                   qoi = "att" ,outcome.var = "y",
                                   lead = 0, forbid.treatment.reversal = FALSE)

      pm <- PanelMatch::PanelEstimate(sets = pm, data = as.data.frame(dat))
      pm_att <- unname(pm[["estimates"]])
      pm_se <- unname(pm[["standard.error"]])

      te_true <- dat%>%
        dplyr::filter(treat == 1)%>%
        dplyr::mutate(effect = te + te_dynamic)%>%
        dplyr::pull(effect)%>%
        mean()

      data.frame(Estimator = c("True Effect","TWFE","CSA","Panel Match"),
                 Estimate = c(te_true,twfe_att,csa_att,pm_att),
                 SE = c(NA,twfe_se,csa_se,pm_se))

    })

    # show data parameters -----------------------------------------------------------------
    output$data_params <- renderTable({

      dat <- gen_dat()

      if(input$is_treated3){
        g3 <- input$g3
        te3 <- input$te3
        te_m3 <- input$te_m3
      }else{
        g3 <- 1000L
        te3 <- 0
        te_m3 <- 0
      }

      data.frame("Group" = c("Group 1", "Group 2", "Group 3"),
                 "TE" = as.character(c(input$te1, input$te2, te3)),
                 "TE Slope" = as.character(c(input$te_m1, input$te_m2, te_m3)))
    })


  }

  shiny::shinyApp(ui, server)
}



