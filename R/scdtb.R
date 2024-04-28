#' Single Case Design Toolbox Shiny Application
#'
#' This Shiny application provides a toolbox for analyzing single case design data.
#' It includes features for data upload, data type handling, raw data visualization,
#' mixed effects analysis, cross-lagged correlation analysis, non-overlap of all pairs
#' analysis, and randomization tests.
#'
#' @return A Shiny app object which can be run to start the application.
#'
#' @examples
#' # To run the Shiny app
#' if(interactive()){
#'   scdtb()
#' }
#'
#' @references
#' Maric, M., & van der Werff, V. (2020). Single-Case ExperimentalDesigns in
#' Clinical Intervention Research. In R. van de Schoot & M. Miocevic (Eds.),
#' Small Sample Size Solutions: A Guide for Applied Researchers and Practitioners
#' (1st ed., pp. 10). Routledge. <doi:10.4324/9780429273872-9>
#'
#' What Works Clearinghouse. (2022). What Works Clearinghouse procedures and
#' standards handbook, version 5.0. U.S. Department of Education, Institute of
#' Education Sciences, National Center for Education Evaluation and Regional
#' Assistance (NCEE). This report is available on the What Works Clearinghouse
#' website at https://ies.ed.gov/ncee/wwc/Handbooks
#'
#' Onghena, P. (2020). One by One: The design and analysis of replicated randomized
#' single-case experiments.In R. van de Schoot & M. Mioecvic (Eds.), Small Sample
#' Size Solutions: A Guide for Applied Researchers and Practitioners (1st ed., pp.
#' 15). Routledge. <doi:10.4324/9780429273872-8>
#'
#' @export
scdtb <- function(){

  # UI
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("lumen"),
    shiny::titlePanel("Set Up SCD Analysis"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("datafile", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
        shiny::h3("Raw Data Plot"),
        shiny::textInput("outcome", "Outcome Variable") |>
          shiny::tags$div(title = "enter the name of the main outcome variable for the analysis"),
        shiny::textInput("time", "Time Variable") |>
          shiny::tags$div(title = "enter the name of the time variable"),
        shiny::textInput("phase", "Phase Variable") |>
          shiny::tags$div(title = "enter the name of the phase variable"),
        shiny::textInput("condition", "Condition Variable") |>
          shiny::tags$div(title = "enter the name of the treatment condition variable"),
        shiny::textInput("participant", "Participant Identifier") |>
          shiny::tags$div(title = "enter the name of the participant identifier"),
        shiny::actionButton("raw_plot_in", "Plot Data"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::h3("Mixed Effect Model"),
        shiny::textInput("cov", "Covariates") |>
          shiny::tags$div(title = "enter a comma separated list of covariates for the mixed effect model"),
        shiny::actionButton("mem", "Mixed Model Analysis"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::h3("Cross-Lagged Correlation"),
        shiny::textInput("xout", "Additional Outcome Variable") |>
          shiny::tags$div(title = "name of the second variable in cross-lagged correlation analysis"),
        shiny::numericInput("lagm", "Lag Max", 5) |>
          shiny::tags$div(title = "maximum lag at which to calculate the cross-correlation"),
        shiny::numericInput("ci_value", "Confidence Interval", value = 0.95, min = 0, max = 1) |>
          shiny::tags$div(title = "confidence interval for cross-lagged correlation and randomization test"),
        shiny::actionButton("clag", "Run Cross-Lagged"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::h3("Non-overlap of All Pairs"),
        shiny::selectInput("naptype", "NAP Type",
                           choices = c("reversability", "trend"),
                           selected = "reversability") |>
          shiny::tags$div(title = "type of analysis to be conducted"),
        shiny::numericInput("lastm", "Last M", NA) |>
          shiny::tags$div(title = "number of measurements from the end to be considered in a trend analysis. Leave as NULL if `NAP Type` is set to 'reversability'"),
        shiny::textInput("napphases", "NAP Phases") |>
          shiny::tags$div(title = "phases to be included in the analysis. If `NAP Type` is 'reversability', two phases (comma-separated) must be specified. If `NAP TYPE` is 'trend', one phase must be specified."),
        shiny::selectInput("imp", "Improvement", c("positive", "negative")) |>
          shiny::tags$div(title = "select the direction of improvement"),
        shiny::actionButton("nap", "Run NAP"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::h3("Randomization Test"),
        shiny::numericInput("perms", "Permutations", NA) |>
          shiny::tags$div(title = "number of permutations to perform. If `NULL`, all possible permutations are considered"),
        shiny::selectInput("cons", "Consecutive", c("observed", "fixed")) |>
          shiny::tags$div(title = "Specifies the constraint on consecutive sequences for permutation. Use `observed` for the observed sequence length or `fixed` for a specified sequence length. Defaults to `observed`"),
        shiny::numericInput("maxc", "Max Consec", NA) |>
          shiny::tags$div(title = "The maximum number of consecutive observations of the same condition to allow in permutations. If `NULL`, no maximum is enforced. To implement `Max Consec`, `Consecutive` must be set to `fixed`."),
        shiny::numericInput("minc", "Min Consec", NA) |>
          shiny::tags$div(title = "The minimum number of consecutive observations of the same condition to allow in permutations. If `NULL`, no minimum is enforced. To implement `Min Consec`, `Consecutive` must be set to `fixed`."),
        shiny::numericInput("bn", "Bins", 30) |>
          shiny::tags$div(title = "number of bins to use for the histogram of the test statistic distribution."),
        shiny::actionButton("rtest", "Run Randomization Test"),
        shiny::br()  # Add a line break
        ),
      shiny::mainPanel(
        shiny::uiOutput("variables_title"),
        DT::dataTableOutput("variables_table"),
        shiny::uiOutput("raw_plot_title"),
        shiny::plotOutput("raw_plot_out"),
        shiny::uiOutput("mem_title"),
        shiny::verbatimTextOutput("mem_out"),
        shiny::plotOutput("mem_plot_out"),
        shiny::uiOutput("cl_title"),
        shiny::verbatimTextOutput("cl_out"),
        shiny::verbatimTextOutput("cl_ci_out"),
        shiny::plotOutput("cl_plot_out"),
        shiny::uiOutput("nap_title"),
        shiny::verbatimTextOutput("nap_out"),
        shiny::uiOutput("rtest_title"),
        shiny::verbatimTextOutput("rtest_diff_out"),
        shiny::verbatimTextOutput("rtest_pval_out"),
        shiny::plotOutput("rtest_plot_out"),
        shiny::verbatimTextOutput("rtest_ci_out")
        )
      )
  )

  # Server
  server <- function(input, output, session){

    ## Set Up Reactive Values

    # reactive: Read the uploaded CSV file
    uploaded_data <- shiny::reactiveVal()

    # reactive expressions for validated/transformed inputs
    reactive_phase <- shiny::reactive({
      if (input$phase != "") { input$phase } else { NULL }
    })

    reactive_condition <- shiny::reactive({
      if (input$condition != "") { input$condition } else { NULL }
    })

    reactive_participant <- shiny::reactive({
      if (input$participant != "") { input$participant } else { NULL }
    })

    reactive_covs <- shiny::reactive({
      if (input$cov != "") { input$cov } else { NULL }
    })

    reactive_napphases <- shiny::reactive({
      if (input$napphases != "") {
        # Split the input string by commas and trim whitespace
        trimws(strsplit(input$napphases, ",\\s*")[[1]])
        } else { NULL }
    })

    reactive_lastm <- shiny::reactive({
      if (!is.na(input$lastm)) { input$lastm } else { NULL }
    })

    reactive_perms <- shiny::reactive({
      if (!is.na(input$perms)) { input$perms } else { NULL }
    })

    reactive_maxc <- shiny::reactive({
      if (!is.na(input$maxc)) { input$maxc } else { NULL }
    })

    reactive_minc <- shiny::reactive({
      if (!is.na(input$minc)) { input$minc } else { NULL }
    })

    ## Data Upload & Handle Data Types

    shiny::observe({
      inFile <- input$datafile
      if (!is.null(inFile)) {
        data <- utils::read.csv(inFile$datapath, stringsAsFactors = TRUE)
        uploaded_data(data)
      }
    })

    output$variables_title <- shiny::renderUI({
      if (!is.null(uploaded_data()) && nrow(uploaded_data()) > 0) {
        shiny::tags$h2("Available Variables")
      }
    })


    output$variables_table <- DT::renderDataTable({
      shiny::req(uploaded_data())
      data <- uploaded_data()
      df <- data.frame(Variable = names(data), Type = sapply(data, class))
      DT::datatable(df, editable = 'cell', options = list(pageLength = 5),
                    rownames = FALSE)
    })

    shiny::observeEvent(input$variables_table_cell_edit, {
      info <- input$variables_table_cell_edit
      shiny::req(uploaded_data())
      data <- uploaded_data()

      row_number <- info$row
      new_value <- info$value

      if (info$col == 0){
        tryCatch({
          names(data)[row_number] <- new_value
          # Update the reactive data frame
          uploaded_data(data)
        }, error = function(e) {
          shiny::showNotification(
            paste("Error in changing variable name:", e$message),
            type = "error",
            duration = NULL
          )
        })
      }

      if (info$col == 1) {  # Assuming the 'Type' column is the second column
        variable_name <- names(data)[row_number]  # Fetch the variable name using row_number
        tryCatch({
          if (new_value == "factor") {
            data[[variable_name]] <- as.factor(data[[variable_name]])
          } else if (new_value == "numeric") {
            data[[variable_name]] <- as.numeric(data[[variable_name]])
          } else if (new_value == "integer") {
            data[[variable_name]] <- as.integer(data[[variable_name]])
          } else if (new_value == "double") {
            data[[variable_name]] <- as.double(data[[variable_name]])
          } else if (new_value == "character") {
            data[[variable_name]] <- as.character(data[[variable_name]])
          } else {
            stop("New data type must be one of the following: factor, numeric, integer, double, character")
          }
          # Update the reactive data frame
          uploaded_data(data)
        }, error = function(e) {
          shiny::showNotification(
            paste("Error in changing data type:", e$message),
            type = "error",
            duration = NULL
          )
        })
      }
    })

    ## Plot Raw Data

    shiny::observeEvent(input$raw_plot_in, {
      shiny::req(uploaded_data(), input$outcome, input$time)

      output$raw_plot_title <- shiny::renderUI({ shiny::tags$h2("Raw Data Plot") })

      output$raw_plot_out <- shiny::renderPlot({
        raw_plot(.df = uploaded_data(), .out = input$outcome,
                 .time = input$time, .phase = reactive_phase(),
                 .cond = reactive_condition(), .participant = reactive_participant())
      })
    })

    ## Run Mixed Effects Analysis

    shiny::observeEvent(input$mem, {
      shiny::req(uploaded_data(), input$outcome, input$time, reactive_phase())

      output$mem_title <- shiny::renderUI({ shiny::tags$h2("Mixed Effects Analysis") })

      mem_res <- mixed_model_analysis(.df = uploaded_data(), .dv = input$outcome,
                                      .time = input$time, .phase = reactive_phase(),
                                      .participant = reactive_participant(),
                                      rev_time_in_phase = FALSE, covs = reactive_covs())

      output$mem_out <- shiny::renderPrint({ summary(mem_res$fitted_mod) })
      output$mem_plot_out <- shiny::renderPlot({ mem_res$plot })
    })

    ## Run Cross-Lagged Correlation Analysis

    shiny::observeEvent(input$clag, {
      shiny::req(uploaded_data(), input$outcome, input$xout)

      cl_res <- cross_lagged(.df = uploaded_data(), .x = input$outcome,
                             .y = input$xout, lag.max = input$lagm,
                             na.action = stats::na.fail, conf.level = input$ci_value)

      output$cl_title <- shiny::renderUI({ shiny::tags$h2("Cross-Lagged Correlation Analysis") })
      output$cl_out <- shiny::renderPrint({ print(cl_res) })
      output$cl_ci_out <- shiny::renderPrint( {print(paste0("Confidence Intervals: [", cl_res$LCI, " , ", cl_res$UCI, "]"))})
      output$cl_plot_out <- shiny::renderPlot({ plot(cl_res) })
    })

    ## Run NAP Analysis

    shiny::observeEvent(input$nap, {
      shiny::req(uploaded_data(), input$outcome, input$time, reactive_phase(),
                 reactive_napphases())

      output$nap_title <- shiny::renderUI({ shiny::tags$h2("Non-overlap of All Pairs Analysis") })
      output$nap_out <- shiny::renderPrint( {

        nap(.df = uploaded_data(), .y = input$outcome, .phase = reactive_phase(),
            .time = input$time, type = input$naptype, last_m = reactive_lastm(),
            phases = reactive_napphases(), improvement = input$imp)

      } )

    })

    ## Run Randomization Test

    shiny::observeEvent(input$rtest, {
      shiny::req(uploaded_data(), input$outcome, reactive_condition(), input$time)

      rand_res <- randomization_test(.df = uploaded_data(), .out = input$outcome,
                                     .cond = reactive_condition(), .time = input$time,
                                     num_permutations = reactive_perms(), consec = input$cons,
                                     max_consec = reactive_maxc(), min_consec = reactive_minc(),
                                     conf.level = input$ci_value, .bins = input$bn)

      output$rtest_title <- shiny::renderUI({ shiny::tags$h2("Randomization Test Analysis") })
      output$rtest_diff_out <- shiny::renderPrint( {paste0("Original Mean Difference: ", rand_res$original_diff)} )
      output$rtest_pval_out <- shiny::renderPrint( {paste0("p-value: ", rand_res$p_value)} )
      output$rtest_plot_out <- shiny::renderPlot({ rand_res$dist_plot })
      output$rtest_ci_out <- shiny::renderPrint( { rand_res$conf_int } )


    })

  }

  shiny::shinyApp(ui = ui, server = server)

}
