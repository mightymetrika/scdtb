scdtb <- function(){

  # UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Set Up SCD Analysis"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("datafile", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
        shiny::textInput("outcome", "Outcome Variable"),
        shiny::textInput("time", "Time Variable"),
        shiny::textInput("phase", "Phase Variable"),
        shiny::textInput("condition", "Condition Variable"),
        shiny::textInput("participant", "Participant Identifier"),
        shiny::textInput("xout", "Additional Outcome Variable"),
        shiny::actionButton("raw_plot_in", "Plot Data"),
        shiny::textInput("cov", "Covariates"),
        shiny::actionButton("mem", "Run Mixed Model"),
        shiny::numericInput("lagm", "Lag Max", 5),
        shiny::numericInput("ci_value", "Confidence Interval", value = 0.95, min = 0, max = 1),
        shiny::actionButton("clag", "Run Cross-Lagged")
        ),
      shiny::mainPanel(
        shiny::uiOutput("variables_title"),
        DT::dataTableOutput("variables_table"),
        shiny::plotOutput("raw_plot_out"),
        shiny::verbatimTextOutput("mem_out"),
        shiny::plotOutput("mem_plot_out"),
        shiny::verbatimTextOutput("cl_out"),
        shiny::verbatimTextOutput("cl_ci_out"),
        shiny::plotOutput("cl_plot_out")
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

      output$raw_plot_out <- shiny::renderPlot({
        raw_plot(.df = uploaded_data(), .out = input$outcome,
                 .time = input$time, .phase = reactive_phase(),
                 .cond = reactive_condition(), .participant = reactive_participant())
      })
    })

    ## Run Mixed Effects Analysis

    shiny::observeEvent(input$mem, {
      shiny::req(uploaded_data(), input$outcome, input$time, reactive_phase())

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

      output$cl_out <- shiny::renderPrint({ print(cl_res) })
      output$cl_ci_out <- shiny::renderPrint( {print(paste0("Confidence Intervals: [", cl_res$LCI, " , ", cl_res$UCI, "]"))})
      output$cl_plot_out <- shiny::renderPlot({ plot(cl_res) })
    })
  }

  shiny::shinyApp(ui = ui, server = server)

}
