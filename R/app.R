library(shiny)
library(DT)
library(dplyr)
library(magrittr)
library(bslib)
library(bsicons)
library(shinyjs)
library(markdown)
# https://excalidraw.com/

source("./sim_data_funs.R") 
source("./fit_spm_funs.R")

scens <- c("healthy", "overfished", "recovering")
hist_catch <- read.csv("./historical_data.csv") %>%
  select(Year, Species, Catch, Scenario, Effort, CPUE) %>%
  # Convert species code names to display names
  mutate(
    Species = case_when(
      Species == "yellowfin_tuna" ~ "Yellowfin Tuna",
      Species == "mahi_mahi" ~ "Mahi Mahi",
      Species == "Opakapaka" ~ "Opakapaka",
      Species == "Peacock_grouper" ~ "Peacock Grouper",
      Species == "Yellowfin_goatfish" ~ "Yellowfin Goatfish",
      TRUE ~ Species
    ),
    # Rename columns to match your table format
    Date = as.character(Year),
    Count = ceiling(Catch),  # Assuming Catch represents count
    Effort = round(Effort, 1)
  ) %>% 
  filter(Scenario %in% scens) %>%
  mutate(Sps_scenario = paste(Species, Scenario, sep = "_"))
  
hist_catch_shiny <- hist_catch %>%
  #filter(scenario == "healthy") %>% # adjust based on what scenario you want to use per species
  select(Species, Scenario, Date, Count, Effort) 

# Create the scaling objects once at app startup
scaling_by_species <- hist_catch_shiny %>%
  group_by(Species, Scenario) %>%
  tidyr::nest() %>%
  mutate(
    # Step A: Generate unique scaling for each species
    scaling_logic = purrr::map(data, ~create_bidirectional_scaling(.x)),
    
    # Step B: Apply scaling logic to the nested data
    formatted_table = purrr::map2(data, scaling_logic, 
      ~display_for_workshop(.x, .y))
  ) %>%
  # Step C: Add both Species and Scenario back into formatted_table
  mutate(formatted_table = purrr::pmap(list(formatted_table, Species, Scenario), 
    function(table, species, scenario) {
      table$Species <- species
      table$Scenario <- scenario
      return(table)
  })) %>%
  ungroup() %>%
  select(formatted_table) %>%
  tidyr::unnest(cols = c(formatted_table)) %>%
  select(Species, Scenario, Date, Count, Effort) %>%
  mutate(Count = round(Count))


ui <- page_sidebar(
  title = "MREP Shiny App",
  theme = bs_theme(bootswatch = "lumen", "navbar-bg" = "#158CBA"),
  
  useShinyjs(),  # Initialize shinyjs

  sidebar = sidebar(
    tags$style(HTML("
      .sidebar-nav-link {
        display: block;
        padding: 12px 15px;
        margin: 5px 0;
        border-radius: 5px;
        color: #333;
        text-decoration: none;
        transition: background-color 0.2s;
        cursor: pointer;
      }
      .sidebar-nav-link:hover {
        background-color: #e9ecef;
      }
      .sidebar-nav-link.active {
        background-color: var(--bs-primary);
        color: white;
      }
    ")),
    
    actionLink("nav_welcome", 
               tagList(bsicons::bs_icon("house"), " Welcome"),
               class = "sidebar-nav-link active"),
    
    actionLink("nav_data_entry", 
               tagList(bsicons::bs_icon("table"), " Data Entry"),
               class = "sidebar-nav-link"),
    
    actionLink("nav_model_results", 
               tagList(bsicons::bs_icon("graph-up"), " Run Model"),
               class = "sidebar-nav-link"),
    
    actionLink("nav_summary", 
               tagList(bsicons::bs_icon("clipboard-data"), " Summary"),
               class = "sidebar-nav-link")
  ),
  
  # Just put the UI output here - don't define it!
  uiOutput("page_content")
)

server <- function(input, output, session) {
  
  current_page <- reactiveVal("welcome")
  fish_data <- reactiveVal(scaling_by_species)
  model_results <- reactiveVal(NULL)
  # A place to store saved plots and results
  saved_results <- reactiveVal(list())

  # Function to update active class
  update_nav <- function(active_page) {
    pages <- c("welcome", "data_entry", "model_results", "summary")
    for (page in pages) {
      if (page == active_page) {
        runjs(sprintf("$('#nav_%s').addClass('active');", page))
      } else {
        runjs(sprintf("$('#nav_%s').removeClass('active');", page))
      }
    }
  }
  
  # Navigation observers
  observeEvent(input$nav_welcome, {
    current_page("welcome")
    update_nav("welcome")
  })
  
  observeEvent(input$nav_data_entry, {
    current_page("data_entry")
    update_nav("data_entry")
  })
  
  observeEvent(input$nav_model_results, {
    current_page("model_results")
    update_nav("model_results")
  })
  
  observeEvent(input$nav_summary, {
    current_page("summary")
    update_nav("summary")
  })
  
  # Render page content - ONLY in server, not in UI!
  output$page_content <- renderUI({
    switch(current_page(),
      
      "welcome" = div(
        shiny::includeMarkdown("./welcome.md")
      ),
      
      "data_entry" = div(
        h3("Catch and Effort Data"),
        
        fluidRow(
          column(3,
            selectInput("filter_species", 
                       "Select Species:", 
                       choices = c("All Species", sort(unique(fish_data()$Species))),
                       selected = "All Species",
                       width = "100%")
          ),
          column(3,
            selectInput("filter_scenario", 
                       "Select Scenario:", 
                       choices = c("All Scenarios", sort(unique(fish_data()$Scenario))),
                       selected = "All Scenarios",
                       width = "100%")
          ),
          column(6,
            div(style = "padding-top: 25px;",
              actionButton("add_data", "Add data", class = "btn-primary"),
              actionButton("edit_row", "Edit row"),
              actionButton("delete_row", "Delete row")
            )
          )
        ),
        br(),
        
        # Data table
        DTOutput("data_table"),
        
        br(),
        
        # Conditional plots - only show when species and scenario are selected
        uiOutput("data_plots_ui")
      ),
      
      "model_results" = div(
        h2("Run Surplus Production Model"),
        
        fluidRow(
          column(12,
            wellPanel(
              p("Select a species and scenario on the Data Entry page, then click the button below to run the model."),
              actionButton("run_model_btn", "Run Model", class = "btn-primary btn-lg", width = "100%")
            )
          )
        ),
        
        fluidRow(
          column(12,
            uiOutput("model_output_ui")
          )
        )
      ),
      
      "summary" = div(
        h2("Summary"),
        p("Let's wrap up what we learned and compare some of our results!"),
        # Show saved plots
        uiOutput("saved_plots_ui"),
        
        hr(),
        p(strong("What were your fishing motivations?")),
        p(strong("Based on what you know about the life history strategy of your species, 
        how do you think your fishing strategy impacted the population?"))
      )
    )
  })
  
 # Render datatable (shows all data)
  output$data_table <- renderDT({
    datatable(
      filtered_data_table()
    )
  })
  
  # Create filtered data for display
  filtered_data_table <- reactive({
    data <- fish_data()
    
    # Filter by species if not "All Species"
    if (input$filter_species != "All Species") {
      data <- data %>% filter(Species == input$filter_species)
    }
    
    # Filter by scenario if not "All Scenarios"
    if (input$filter_scenario != "All Scenarios") {
      data <- data %>% filter(Scenario == input$filter_scenario)
    }
    
    return(data)
  })

   # Conditional UI for plots - only show when specific species and scenario selected
  output$data_plots_ui <- renderUI({
    # Only show plots if specific species and scenario are selected
    if (input$filter_species == "All Species" || input$filter_scenario == "All Scenarios") {
      return(
        div(
          style = "padding: 20px; text-align: center; color: #666;",
          p("Select a specific species and scenario to view plots")
        )
      )
    }
    
    # Show the plots
    fluidRow(
      column(6,
        plotOutput("catch_plot", height = "400px")
      ),
      column(6,
        plotOutput("cpue_plot", height = "400px")
      )
    )
  })
  
  # Render catch plot
  output$catch_plot <- renderPlot({
    req(input$filter_species != "All Species")
    req(input$filter_scenario != "All Scenarios")
    
    data <- filtered_data_table()
    req(nrow(data) > 0)
    
    plot_catch_data(data)
  })
  
  # Render CPUE plot
  output$cpue_plot <- renderPlot({
    req(input$filter_species != "All Species")
    req(input$filter_scenario != "All Scenarios")
    
    data <- filtered_data_table()
    req(nrow(data) > 0)
    
    plot_cpue(data)
  })

  # Display saved plots
  output$saved_plots_ui <- renderUI({
    saved <- saved_results()
    
    if (length(saved) == 0) {
      return(
        div(
          style = "padding: 20px; text-align: center; color: #666;",
          icon("chart-bar", style = "font-size: 48px;"),
          h4("No saved plots yet"),
          p("Run models and click 'Send to Summary' to save plots here.")
        )
      )
    }
    
    # Create a card for each saved plot
    plot_cards <- lapply(names(saved), function(result_id) {
      result <- saved[[result_id]]
      
      # Create unique output ID
      plot_output_id <- paste0("saved_plot_", gsub("[^[:alnum:]]", "_", result_id))
      
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(
            strong(result$title),
            br(),
            span(style = "font-size: 0.9em; color: #666;",
                format(result$timestamp, "%Y-%m-%d %H:%M:%S"))
          ),
          actionButton(
            inputId = paste0("delete_", result_id),
            label = icon("trash"),
            class = "btn-danger btn-sm",
            onclick = sprintf("Shiny.setInputValue('delete_plot', '%s', {priority: 'event'})", result_id)
          )
        ),
        card_body(
          plotOutput(plot_output_id, height = "500px")
        )
      )
    })
    
    # Arrange in a grid
    layout_column_wrap(
      width = 1/2,  # Two columns
      !!!plot_cards
    )
  })

  # Render each saved plot
  observe({
    saved <- saved_results()
    
    lapply(names(saved), function(result_id) {
      result <- saved[[result_id]]
      plot_output_id <- paste0("saved_plot_", gsub("[^[:alnum:]]", "_", result_id))
      
      output[[plot_output_id]] <- renderPlot({
        if (result$plot_type == "kobe") {
          plot_kobe(result$model_fit)
        } else if (result$plot_type == "biomass") {
          plot_biomass_trajectory(result$model_fit)
        }
      })
    })
  })

  # Delete saved plot
  observeEvent(input$delete_plot, {
    result_id <- input$delete_plot
    
    current_saved <- saved_results()
    current_saved[[result_id]] <- NULL
    saved_results(current_saved)
    
    showNotification("Plot removed from Summary", type = "message", duration = 2)
  })

   # Run model when button is clicked
  observeEvent(input$run_model_btn, {
    
    # Check if a specific species is selected
    if (input$filter_species == "All Species") {
      showNotification("Please select a specific species on the Data Entry page", type = "warning", duration = 5)
      return()
    }
    
    # Show progress notification
    showNotification("Running model...", id = "model_run", duration = NULL, type = "message")
    
    # Use the already filtered data from the data entry page
    species_data <- filtered_data_table()
    
    # Check if there's data
    if (nrow(species_data) == 0) {
      removeNotification("model_run")
      showNotification(paste("No data available for", input$filter_species, 
                            ifelse(input$filter_scenario != "All Scenarios", 
                                   paste("with scenario", input$filter_scenario), "")), 
                      type = "error")
      return()
    }
    # Run the surplus production model
    tryCatch({
      model_fit <- fit_schaefer_model(
        data = species_data,
        use_cpue = TRUE
      )
      
      results <- list(
        species = input$filter_species,
        scenario = input$filter_scenario,
        n_observations = nrow(species_data),
        data = species_data,
        model_fit = model_fit  
      )
      
      model_results(results)
      
      removeNotification("model_run")
      showNotification("Model run complete!", type = "message", duration = 3)
      
    }, error = function(e) {
      removeNotification("model_run")
      showNotification(paste("Error running model:", e$message), type = "error")
    })
  })
  
  # Display model results
  output$model_output_ui <- renderUI({
    results <- model_results()
    
    if (is.null(results)) {
      return(
        div(
          style = "padding: 20px; text-align: center; color: #666;",
          icon("info-circle", style = "font-size: 48px;"),
          h4("No model results yet"),
          p("Select a species and click 'Run Model' to see results here.")
        )
      )
    }

    # Create title with scenario if applicable
    title <- paste("Model Results:", results$species, "-", results$scenario)
    
    # Display results
    div(
      wellPanel(
        h3(title),
        hr(),
        
        hr(),
        h4("Model Parameters & Stock Status"),
        verbatimTextOutput("model_summary"),
        
      hr(),
      fluidRow(
        column(10,
          h4("Kobe Plot - Stock Status")
        ),
        column(2,
          actionButton("save_kobe", "Send to Summary", 
                      class = "btn-success btn-sm",
                      style = "margin-top: 10px;")
        )
      ),
      plotOutput("kobe_plot", height = "600px"),
        
      hr(),
      fluidRow(
        column(10,
          h4("Biomass Trajectory")
        ),
        column(2,
          actionButton("save_biomass", "Send to Summary", 
                      class = "btn-success btn-sm",
                      style = "margin-top: 10px;")
        )
      ),
      plotOutput("biomass_plot", height = "600px")
      )
    )
  })
  
  # Show the filtered data used in model
  output$model_data_table <- renderDT({
    results <- model_results()
    req(results)
    
    datatable(
      results$data,
      options = list(pageLength = 5)
    )
  })
  
  # Show model summary
  output$model_summary <- renderPrint({
    results <- model_results()
    req(results)
    req(results$model_fit)
    
    model_fit <- results$model_fit
    
    cat("=== MODEL FIT SUMMARY ===\n\n")
    cat("Species:", results$species, "\n")
    cat("Data points:", results$n_observations, "\n\n")
    
    cat("--- Estimated Parameters ---\n")
    cat(sprintf("  r (intrinsic growth rate): %.4f\n", model_fit$parameters$r))
    cat(sprintf("  K (carrying capacity): %.2f\n", model_fit$parameters$K))
    cat(sprintf("  q (catchability): %.6f\n\n", model_fit$parameters$q))
    
    cat("--- Reference Points ---\n")
    cat(sprintf("  B_MSY: %.2f\n", model_fit$reference_points$Bmsy))
    cat(sprintf("  MSY: %.2f\n", model_fit$reference_points$MSY))
    cat(sprintf("  F_MSY: %.4f\n\n", model_fit$reference_points$Fmsy))
    
    cat("--- Current Stock Status ---\n")
    cat(sprintf("  Current Biomass: %.2f\n", model_fit$current_status$B_current))
    cat(sprintf("  B/B_MSY: %.3f\n", model_fit$current_status$B_Bmsy))
    cat(sprintf("  Current Fishing Mortality: %.4f\n", model_fit$current_status$F_current))
    cat(sprintf("  F/F_MSY: %.3f\n", model_fit$current_status$F_Fmsy))
    cat(sprintf("  Stock Status: %s\n\n", model_fit$current_status$status))
    
  })

  output$kobe_plot <- renderPlot({
    results <- model_results()
    req(results)
    req(results$model_fit)
    
    model_fit <- results$model_fit

    plot_kobe(model_fit)
  })

  output$biomass_plot <- renderPlot({
    results <- model_results()
    req(results)
    req(results$model_fit)
    
    model_fit <- results$model_fit

    plot_biomass_trajectory(model_fit)
  })

  observeEvent(input$add_data, {
    showModal(modalDialog(
      title = "Add data",
      div(class = "modal-section",
        h4("Species"),
        selectInput("add_species", NULL, choices = c("Yellowfin_tuna", "Mahi_Mahi", "Opakapaka", "Peacock_grouper", "Yellowfin_goatfish"), selected = "Yellowfin_tuna", width = "100%")
      ),
      div(class = "modal-section",
        h4("Scenario"),
        selectInput("add_scenario", NULL, choices = c("healthy", "overfished", "recovering", "declining"), selected = "healthy", width = "100%")
      ),
      div(class = "modal-section",
        h4("Date"),
        numericInput("add_date", NULL, value = 1, width = "100%")
      ),
      div(class = "modal-section",
        h4("Count"),
        numericInput("add_count", NULL, value = 1, width = "100%")
      ),
      div(class = "modal-section",
        h4("Effort"),
        numericInput("add_effort", NULL, value = 1, width = "100%")
      ),
      footer = tagList(modalButton("Cancel"), actionButton("submit_data", "Add Data", class = "btn-primary")),
      size = "m",
      easyClose = TRUE
    ))
  })

    observeEvent(input$submit_data, {
    current_data <- fish_data()
    
    new_row <- data.frame(
      Species = input$add_species,
      Scenario = input$add_scenario,
      Date = input$add_date,
      Count = input$add_count,
      Effort = input$add_effort
    )

    updated_data <- rbind(current_data, new_row)
    fish_data(updated_data)
    
    showNotification("Data added successfully!", type = "message")
    removeModal()
  })

  observeEvent(input$edit_row, {
    selected <- input$data_table_rows_selected
    
    if (is.null(selected) || length(selected) == 0) {
      showNotification("Please select a row to edit", type = "warning")
      return()
    }
    
    current_data <- fish_data()
    row_data <- current_data[selected, ]
    
    showModal(modalDialog(
      title = "Edit Row",
      div(class = "modal-section",
        h4("Species"),
        selectInput("edit_species", NULL, choices = c("Yellowfin_tuna", "Mahi_Mahi", "Opakapaka", "Peacock_grouper", "Yellowfin_goatfish"), selected = row_data$Species, width = "100%")
      ),
      div(class = "modal-section",
        h4("Scenario"),
        selectInput("edit_scenario", NULL, c("healthy", "overfished", "recovering", "declining"), selected = row_data$Scenario, width = "100%")
      ),
      div(class = "modal-section",
        h4("Date"),
        numericInput("edit_date", NULL, value = row_data$Date, width = "100%")
      ),
      div(class = "modal-section",
        h4("Count"),
        numericInput("edit_count", NULL, value = row_data$Count, min = 0, width = "100%")
      ),
      div(class = "modal-section",
        h4("Effort"),
        numericInput("edit_effort", NULL, value = row_data$Effort, min = 0, step = 0.5, width = "100%")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edit", "Save Changes", class = "btn-primary")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$save_edit, {
    selected <- input$data_table_rows_selected
    current_data <- fish_data()
    current_data$Date <- as.numeric(current_data$Date)
    
    current_data[selected, "Species"] <- input$edit_species
    current_data[selected, "Scenario"] <- input$edit_scenario
    current_data[selected, "Date"] <- input$edit_date
    current_data[selected, "Count"] <- input$edit_count
    current_data[selected, "Effort"] <- input$edit_effort
    
    fish_data(current_data)
    
    showNotification("Row updated successfully!", type = "message")
    removeModal()
  })
  
  observeEvent(input$delete_row, {
    selected <- input$data_table_rows_selected
    
    if (is.null(selected) || length(selected) == 0) {
      showNotification("Please select a row to delete", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "Delete Row",
      "Are you sure you want to delete this row? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_delete, {
    selected <- input$data_table_rows_selected
    current_data <- fish_data()
    
    current_data <- current_data[-selected, ]
    fish_data(current_data)
    
    showNotification("Row deleted successfully!", type = "message")
    removeModal()
  })

  # Save Kobe plot to summary
observeEvent(input$save_kobe, {
  results <- model_results()
  req(results)
  
  # Create a unique ID for this result
  result_id <- paste0(results$species, "_", results$scenario, "_kobe_", 
                      format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  # Store the plot information
  new_result <- list(
    id = result_id,
    species = results$species,
    scenario = results$scenario,
    plot_type = "kobe",
    model_fit = results$model_fit,
    timestamp = Sys.time(),
    title = paste("Kobe Plot:", results$species, "-", results$scenario)
  )
  
  # Add to saved results
  current_saved <- saved_results()
  current_saved[[result_id]] <- new_result
  saved_results(current_saved)
  
  showNotification("Kobe plot saved to Summary page!", 
                  type = "message", duration = 3)
})

  # Save Biomass plot to summary
  observeEvent(input$save_biomass, {
    results <- model_results()
    req(results)
    
    # Create a unique ID for this result
    result_id <- paste0(results$species, "_", results$scenario, "_biomass_", 
                        format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    # Store the plot information
    new_result <- list(
      id = result_id,
      species = results$species,
      scenario = results$scenario,
      plot_type = "biomass",
      model_fit = results$model_fit,
      timestamp = Sys.time(),
      title = paste("Biomass Trajectory:", results$species, "-", results$scenario)
    )
    
    # Add to saved results
    current_saved <- saved_results()
    current_saved[[result_id]] <- new_result
    saved_results(current_saved)
    
    showNotification("Biomass plot saved to Summary page!", 
                    type = "message", duration = 3)
  })

}

shinyApp(ui = ui, server = server)