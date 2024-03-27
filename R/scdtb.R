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
        ),
      shiny::mainPanel(
        shiny::uiOutput("variables_title"),
        DT::dataTableOutput("variables_table"),
        shiny::plotOutput("raw_plot_out"),
        )
      )
  )

  # Server
  server <- function(input, output, session){

    # Reactive: Read the uploaded CSV file
    uploaded_data <- shiny::reactiveVal()

    # Reactive expressions for validated/transformed inputs
    reactive_phase <- shiny::reactive({
      if (input$phase != "") { input$phase } else { NULL }
    })

    reactive_condition <- shiny::reactive({
      if (input$condition != "") { input$condition } else { NULL }
    })

    reactive_participant <- shiny::reactive({
      if (input$participant != "") { input$participant } else { NULL }
    })

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

    # Get raw plot
    # shiny::observeEvent(input$raw_plot_in, {
    #   shiny::req(uploaded_data(), input$outcome, input$time)
    #
    #   output$raw_plot_out <- shiny::renderPlot({
    #     .phase <- if (input$phase != "") input$phase else NULL
    #     .cond <- if (input$condition != "") input$condition else NULL
    #     .participant <- if (input$participant != "") input$participant else NULL
    #
    #     raw_plot(.df = uploaded_data(), .out = input$outcome,
    #              .time = input$time, .phase = .phase, .cond = .cond,
    #              .participant = .participant)
    #   })
    # })

    shiny::observeEvent(input$raw_plot_in, {
      shiny::req(uploaded_data(), input$outcome, input$time)

      output$raw_plot_out <- shiny::renderPlot({
        raw_plot(.df = uploaded_data(), .out = input$outcome,
                 .time = input$time, .phase = reactive_phase(),
                 .cond = reactive_condition(), .participant = reactive_participant())
      })
    })

  }

  shiny::shinyApp(ui = ui, server = server)

}
