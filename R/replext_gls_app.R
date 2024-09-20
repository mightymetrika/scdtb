#' Replication and Extension of Generalized Least Squares Simulation Shiny App
#'
#' This function creates a Shiny app for running and visualizing simulations of
#' Generalized Least Squares (GLS) models, extending the work of Maric et al. (2014).
#' It allows users to interactively set simulation parameters, run simulations,
#' view results, and generate plots.
#'
#' @param dbname Character string. The name of the PostgreSQL database to connect to.
#' @param datatable Character string. The name of the table in the database where
#'     results will be stored.
#' @param host Character string. The host name or IP address of the PostgreSQL server.
#' @param port Integer. The port number on which the PostgreSQL server is listening.
#' @param user Character string. The username for the PostgreSQL database connection.
#' @param password Character string. The password for the PostgreSQL database
#'     connection.
#'
#' @return A Shiny app object which can be run to start the application.
#'
#' @details
#' The app provides a user interface for:
#' \itemize{
#'   \item Setting simulation parameters
#'   \item Running simulations based on the specified parameters
#'   \item Viewing simulation results in a table format
#'   \item Generating plots of various metrics across different conditions
#'   \item Storing and retrieving results from a PostgreSQL database
#'   \item Displaying relevant citations
#' }
#'
#' The app uses the \code{replext_gls()} function to perform the actual simulations.
#'
#' @references
#' Maric, M., de Haan, E., Hogendoorn, S.M., Wolters, L.H. & Huizenga, H.M. (2014).
#' Evaluating statistical and clinical significance of intervention effects in
#' single-case experimental designs: An SPSS method to analyze univariate data.
#' Behavior Therapy. doi: 10.1016/j.beth.2014.09.009
#'
#' @seealso \code{\link{replext_gls}} for the underlying simulation function.
#'
#' @examples
#' if(interactive()){
#' replext_pgsql(
#'   dbname = "my_database",
#'   datatable = "simulation_results",
#'   host = "localhost",
#'   port = 5432,
#'   user = "myuser",
#'   password = "mypassword"
#' )
#' }
#'
#' @export
replext_pgsql <- function(dbname, datatable, host, port, user, password) {

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        mmints::postgresUI("postgres")$submit,
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        mmints::postgresUI("postgres")$download,
        mmints::citationUI("citations")$button
      ),
      shiny::mainPanel(
        # Conditionally display the Simulation Results header and table
        shiny::uiOutput("simulation_results_header"),
        DT::DTOutput("resultsTable"),
        shiny::br(),  # Add a line break
        shiny::uiOutput("plot_controls"),
        shiny::plotOutput("simulationPlot"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        # Add a header for the responses table
        shiny::div(
          shiny::h4("All Responses"),
          mmints::postgresUI("postgres")$table,
        ),
        shiny::uiOutput("citation_header"),
        mmints::citationUI("citations")$output
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {

    # Render the UI for parameters based on the selected cell block
    output$paramsUI <- shiny::renderUI({
      replext_gls_UIParams()
    })

    # initialize the postgres module
    postgres_module <- mmints::postgresServer("postgres",
                                              dbname = dbname,
                                              datatable = datatable,
                                              host = host,
                                              port = port,
                                              user = user,
                                              password = password,
                                              data = NULL)

    # Reactive value to store the results
    results <- shiny::reactiveVal(data.frame())     #For display
    results_exp <- shiny::reactiveVal(data.frame()) #For export

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {

      # make sure responses are clear
      results(data.frame())

      # handle NULL values in covariate_specs
      if (input$covariate_specs == "NULL"){
        .covariate_specs <- NULL
      } else {
        .covariate_specs <- mmints::text_to_list(input$covariate_specs)
      }

      # Call the simulation function with both user-provided and default parameters
      simResults <- replext_gls(n_timepoints_list = mmints::text_to_vector(input$n_timepoints_list),
                                rho_list = mmints::text_to_vector(input$rho_list),
                                iterations = input$iterations,
                                n_phases = input$n_phases,
                                n_IDs = input$n_IDs,
                                betas = unlist(mmints::text_to_list(input$betas)),
                                formula = stats::as.formula(input$formula),
                                covariate_specs = .covariate_specs,
                                alpha_level = input$alpha,
                                verbose = FALSE)

      # remove rownames
      rownames(simResults) <- NULL

      # Update the results reactive value
      results(simResults)
      results_exp(results())

      # submit results to database
      postgres_module$data_to_submit(results_exp())
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

    # After the results are updated, create the UI outputs
    output$plot_controls <- shiny::renderUI({
      if (nrow(results()) > 0) {
        # Extract unique terms and metrics from results()
        terms <- unique(results()$term)
        # Define the metrics you want to allow users to select
        metrics <- c("rejection_rates", "se_rejection_rates", "mean_rmse", "mean_bias",
                     "mean_estimates", "se_estimates", "success_rate")
        shiny::tagList(
          shiny::selectInput("selected_term", "Select Term:", choices = terms),
          shiny::selectInput("selected_metric", "Select Metric:", choices = metrics)
        )
      } else {
        NULL
      }
    })

    # Render the plot based on user selections
    output$simulationPlot <- shiny::renderPlot({
      if (nrow(results()) > 0 && !is.null(input$selected_term) && !is.null(input$selected_metric)) {
        plot(results(), term = input$selected_term, metric = input$selected_metric)
      }
    })


    # build citation list
    citations <- list(
      "Original Simulation:" = "Maric, M., de Haan, E., Hogendoorn, S.M., Wolters, L.H. & Huizenga, H.M. (2014). Evaluating statistical and clinical significance of intervention effects in single-case experimental designs: An SPSS method to analyze univariate data. Behavior Therapy. doi: 10.1016/j.beth.2014.09.009",
      "Software Implementing Generalized Least Squares:" = function() mmints::format_citation(utils::citation("nlme")[1]),
      " " = function() mmints::format_citation(utils::citation("nlme")[2]),
      "Replext Simulation App:" = function() mmints::format_citation(utils::citation("scdtb"))
    )

    # create citation for display
    mmints::citationServer("citations", citations)

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
