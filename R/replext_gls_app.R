# replext_pgsql <- function(dbname, datatable, host, port, user, password) {
replext_gls_app <- function() {

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # shiny::selectInput("cellBlock", "Select Cell Block:",
        #                    choices = getCellBlocks()),
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        # mmints::postgresUI("postgres")$submit,
        # shiny::br(),  # Add a line break
        # shiny::br(),  # Add a line break
        # mmints::postgresUI("postgres")$download,
        # mmints::citationUI("citations")$button
      ),
      shiny::mainPanel(
        # Conditionally display the Simulation Results header and table
        shiny::uiOutput("simulation_results_header"),
        DT::DTOutput("resultsTable"),
        # shiny::br(),  # Add a line break
        # shiny::br(),  # Add a line break
        # # Add a header for the responses table
        # shiny::div(
        #   shiny::h4("All Responses"),
        #   mmints::postgresUI("postgres")$table,
        # ),
        # shiny::uiOutput("citation_header"),
        # mmints::citationUI("citations")$output
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {

    # Render the UI for parameters based on the selected cell block
    output$paramsUI <- shiny::renderUI({
      # getUIParams(input$cellBlock)
      replext_gls_UIParams()
    })

    # initialize the postgres module
    # postgres_module <- mmints::postgresServer("postgres",
    #                                           dbname = dbname,
    #                                           datatable = datatable,
    #                                           host = host,
    #                                           port = port,
    #                                           user = user,
    #                                           password = password,
    #                                           data = NULL)

    # Reactive value to store the results
    results <- shiny::reactiveVal(data.frame())     #For display
    results_exp <- shiny::reactiveVal(data.frame()) #For export

    # # Load data from the database on app start
    # output$responses <- DT::renderDT({
    #   loadData()
    # }, options = list(pageLength = 5))

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {

      # make sure responses are clear
      results(data.frame())

      # Call the simulation function with both user-provided and default parameters
      # simResults <- runSimulation(input)

      if (input$covariate_specs == "NULL"){
        .covariate_specs <- NULL
      } else {
        .covariate_specs <- mmints::text_to_list(input$covariate_specs)
      }

      simResults <- replext_gls(n_timepoints_list = mmints::text_to_vector(input$n_timepoints_list),
                                rho_list = mmints::text_to_vector(input$rho_list),
                                iterations = input$iterations,
                                n_phases = input$n_phases,
                                n_IDs = input$n_IDs,
                                betas = unlist(mmints::text_to_list(input$betas)),
                                formula = stats::as.formula(input$formula),
                                covariate_specs = .covariate_specs,
                                # covariate_specs = NULL,
                                alpha_level = input$alpha,
                                verbose = FALSE)

      # Update the results reactive value
      results(simResults)
      # results_exp(appendInputParams(results(), input))

      # submit results to database
      # postgres_module$data_to_submit(results_exp())
    })

    #Output the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))

    # Conditionally display the Simulation Results header
    output$simulation_results_header <- shiny::renderUI({
      if (nrow(results()) > 0) {
        shiny::h4("Simulation Results")
      } else {
        NULL
      }
    })

    # # build citation list
    # citations <- list(
    #   "Nonparametric Bootstrap Test with Pooled Resampling Method:" = "Dwivedi, A. K., Mallawaarachchi, I., & Alvarado, L. A. (2017). Analysis of small sample size studies using nonparametric bootstrap test with pooled resampling method. Statistics in Medicine, 36(14), 2187-2205. https://doi.org/10.1002/sim.7263",
    #   "Software Implementing Nonparametric Bootstrap Test with Pooled Resampling:" = function() mmints::format_citation(utils::citation("npboottprm"))
    # )
    #
    # # create citation for display
    # mmints::citationServer("citations", citations)

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
