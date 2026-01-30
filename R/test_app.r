library(shiny)
library(DT)
library(dplyr)
library(magrittr)
library(bslib)
library(bsicons)
library(shinyjs)
# https://excalidraw.com/

source("../sim_data_funs.R") 
source("../fit_spm_funs.R")

hist_catch <- read.csv("./historical_data.csv") %>%
  select(Year, SPECIES, Catch, scenario, Effort, CPUE) %>%
  # Convert species code names to display names
  mutate(
    SPECIES = case_when(
      SPECIES == "yellowfin_tuna" ~ "Yellowfin Tuna",
      SPECIES == "mahi_mahi" ~ "Mahi Mahi",
      SPECIES == "Opakapaka" ~ "Opakapaka",
      SPECIES == "Peacock_grouper" ~ "Peacock Grouper",
      SPECIES == "Yellowfin_goatfish" ~ "Yellowfin Goatfish",
      TRUE ~ SPECIES
    ),
    # Rename columns to match your table format
    Date = as.character(Year),
    Count = round(Catch),  # Assuming Catch represents count
    Effort = round(Effort, 1)
  ) %>% 
  mutate(Sps_scenario = paste(SPECIES, scenario, sep = "_"))
  
hist_catch_shiny <- hist_catch %>%
  filter(scenario == "healthy") %>% # adjust based on what scenario you want to use per species
  select(SPECIES, Date, Count, Effort) 

# Create the scaling objects once at app startup
scaling_by_species <- hist_catch_shiny %>%
  group_by(SPECIES) %>%
  tidyr::nest() %>%
  mutate(
    # Step A: Generate unique scaling for each species
    scaling_logic = purrr::map(data, ~create_bidirectional_scaling(.x)),
    
    # Step B: Apply scaling logic to the nested data
    formatted_table = purrr::map2(data, scaling_logic, ~display_for_workshop(.x, .y))
  ) %>%
  # Step C: Crucial part - ensure the SPECIES name is inside the formatted_table
  # before we unnest it
  mutate(formatted_table = purrr::map2(formatted_table, SPECIES, ~{
    .x$SPECIES <- .y  # Assign the group name back into the dataframe
    return(.x)
  })) %>%
  ungroup() %>%
  select(formatted_table) %>%
  tidyr::unnest(cols = c(formatted_table)) %>%
  select(SPECIES, Date, Marbles, Seconds)


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
        h2("Welcome to MREP Shiny App"),
        p("This application allows you to manage catch and effort data for marine species.")
      ),
      
      "data_entry" = div(
        style = "height: calc(100vh - 100px); display: flex; flex-direction: column;",
        h3("Catch and Effort Data"),
        
        fluidRow(
          column(12,
            actionButton("add_data", "Add data", class = "btn-primary"),
            actionButton("edit_row", "Edit row"),
            actionButton("delete_row", "Delete row")
          )
        ),
        br(),
        
        div(style = "flex: 1; overflow: auto;",
          DTOutput("data_table")
        )
      ),
      
      "model_results" = div(
        h2("Run Surplus Production Model"),
        
        fluidRow(
          column(4,
            wellPanel(
              h4("Model Settings"),
              selectInput("model_species", 
                         "Select Species:", 
                         choices = sort(unique(fish_data()$SPECIES)),
                         selected = sort(unique(fish_data()$SPECIES))[1],
                         width = "100%"),
              br(),
              actionButton("run_model_btn", "Run Model", class = "btn-primary btn-lg", width = "100%")
            )
          ),
          column(8,
            uiOutput("model_output_ui")
          )
        )
      ),
      
      "summary" = div(
        h2("Summary"),
        p("Summary statistics will be displayed here.")
      )
    )
  })
  
  # Render datatable
  output$data_table <- renderDT({
    datatable(
      fish_data()
    )
  })

  # Run model when button is clicked
  observeEvent(input$run_model_btn, {
    req(input$model_species)
    
    # Show progress notification
    showNotification("Running model...", id = "model_run", duration = NULL, type = "message")
    
    # Filter data for selected species
    species_data <- fish_data() %>%
      filter(SPECIES == input$model_species)
    
    # Check if there's data for this species
    if (nrow(species_data) == 0) {
      removeNotification("model_run")
      showNotification(paste("No data available for", input$model_species), type = "error")
      return()
    }
    
    # Run the surplus production model
    # This is where you'll call your fit_spm_funs.R functions
    tryCatch({
      # Example: Assuming you have a function like fit_spm() that takes the data
      # results <- fit_spm(species_data)
      
      # For now, placeholder results
      results <- list(
        species = input$model_species,
        n_observations = nrow(species_data),
        data = species_data,
        # Add your actual model results here
        model_fit = NULL  # Replace with actual model output
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
    
    # Display results
    div(
      wellPanel(
        h3(paste("Model Results:", results$species)),
        hr(),
        h4("Input Data Summary"),
        p(paste("Number of observations:", results$n_observations)),
        DTOutput("model_data_table"),
        hr(),
        h4("Model Output"),
        p("Model fit results will be displayed here."),
        # Add your model output visualizations/tables here
        verbatimTextOutput("model_summary")
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
    
    # Replace with actual model summary
    cat("Model fit summary:\n")
    cat(paste("Species:", results$species, "\n"))
    cat(paste("Data points:", results$n_observations, "\n"))
    cat("\n[Model parameters and fit statistics will appear here]\n")
  })

  observeEvent(input$add_data, {
    showModal(modalDialog(
      title = "Add data",
      div(class = "modal-section",
        h4("SPECIES"),
        selectInput("add_species", NULL, choices = c("Yellowfin Tuna", "Mahi Mahi", "Opakapaka", "Peacock Grouper", "Yellowfin Goatfish"), selected = "Yellowfin Tuna", width = "100%")
      ),
      div(class = "modal-section",
        h4("Date"),
        numericInput("add_date", NULL, value = 1, width = "100%")
      ),
      div(class = "modal-section",
        h4("Marbles"),
        numericInput("add_count", NULL, value = 1, width = "100%")
      ),
      div(class = "modal-section",
        h4("Seconds"),
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
      SPECIES = input$add_species,
      Date = input$add_date,
      Marbles = input$add_count,
      Seconds = input$add_effort
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
        h4("SPECIES"),
        selectInput("edit_species", NULL, choices = c("Yellowfin Tuna", "Mahi Mahi", "Opakapaka", "Peacock Grouper", "Yellowfin Goatfish"), selected = row_data$SPECIES, width = "100%")
      ),
      div(class = "modal-section",
        h4("Date"),
        numericInput("edit_date", NULL, value = row_data$Date, width = "100%")
      ),
      div(class = "modal-section",
        h4("Marbles"),
        numericInput("edit_count", NULL, value = row_data$Marbles, min = 0, width = "100%")
      ),
      div(class = "modal-section",
        h4("Seconds"),
        numericInput("edit_effort", NULL, value = row_data$Seconds, min = 0, step = 0.5, width = "100%")
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
    
    current_data[selected, "SPECIES"] <- input$edit_species
    current_data[selected, "Date"] <- input$edit_date
    current_data[selected, "Marbles"] <- input$edit_count
    current_data[selected, "Seconds"] <- input$edit_effort
    
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

}

shinyApp(ui = ui, server = server)