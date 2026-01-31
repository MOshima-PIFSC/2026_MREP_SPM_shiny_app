library(shiny)
library(DT)
library(dplyr)
library(magrittr)
library(bslib)
library(bsicons)
#library(shinyjs)
# https://excalidraw.com/

hist_catch <- read.csv("./historical_data.csv") %>%
  select(Year, Species, Catch, scenario, Effort, CPUE) %>%
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
    Count = round(Catch),  # Assuming Catch represents count
    Effort = round(Effort, 1)
  ) %>% 
  mutate(Sps_scenario = paste(Species, scenario, sep = "_"))
  
hist_catch_shiny <- hist_catch %>%
  filter(scenario == "healthy") %>% # adjust based on what scenario you want to use per species
  select(Species, Date, Count, Effort) 
  #rename(`Effort (sec)` = Effort)


ui <- page_sidebar(
  title = "MREP Shiny App",
  theme = bs_theme(bootswatch = "lumen"),  # Modern theme
  
  sidebar = sidebar(
    width = 280,
    
    accordion(
      accordion_panel(
        "Navigation",
        icon = bsicons::bs_icon("compass"),
        radioButtons(
          "page_select",
          label = NULL,
          choiceNames = list(
            tagList(bsicons::bs_icon("house"), " Welcome"),
            tagList(bsicons::bs_icon("table"), " Data Entry"),
            tagList(bsicons::bs_icon("graph-up"), " Model Results"),
            tagList(bsicons::bs_icon("clipboard-data"), " Summary")
          ),
          choiceValues = c("welcome", "data_entry", "model_results", "summary"),
          selected = "welcome"
        )
      )#,
      
      # accordion_panel(
      #   "Settings",
      #   icon = bsicons::bs_icon("gear"),
      #   selectInput("species", "Species:", 
      #               choices = c("Tuna", "Mahi", "Opaka")),
      #   selectInput("scenario", "Scenario:",
      #               choices = c("Healthy", "Overfished"))
      # )
    )
  ),
  
  uiOutput("page_content")
)

server <- function(input, output, session) {
  
  current_page <- reactiveVal("welcome")
  
  fish_data <- reactiveVal(hist_catch_shiny)
  
  output$nav_menu <- renderUI({
    page <- current_page()
    tagList(
      actionLink("nav_welcome", div(class = paste("nav-item", if(page == "welcome") "active" else ""), "Welcome"), style = "text-decoration: none; color: inherit;"),
      actionLink("nav_data", div(class = paste("nav-item", if(page == "data") "active" else ""), "Data Entry"), style = "text-decoration: none; color: inherit;"),
      actionLink("nav_model", div(class = paste("nav-item", if(page == "model") "active" else ""), "Model Results"), style = "text-decoration: none; color: inherit;"),
      actionLink("nav_summary", div(class = paste("nav-item", if(page == "summary") "active" else ""), "Summary"), style = "text-decoration: none; color: inherit;")
    )
  })
  
  observeEvent(input$nav_welcome, { current_page("welcome") })
  observeEvent(input$nav_data, { current_page("data") })
  observeEvent(input$nav_model, { current_page("model") })
  observeEvent(input$nav_summary, { current_page("summary") })
  
  output$page_content <- renderUI({
    page <- current_page()
    
    if (page == "welcome") {
      div(class = "table-section",
        h2("Welcome to MREP Shiny App"),
        p("This application allows you to manage catch and effort data for marine species."),
        hr(),
        h4("Features:"),
        tags$ul(
          tags$li("Data Entry: Add, edit, and delete catch records"),
          tags$li("Model Results: View analysis results"),
          tags$li("Summary: Review summary statistics and reports")
        ),
        p("Use the navigation panel on the left to get started.")
      )
    } else if (page == "data") {
      div(class = "table-section",
        div(class = "section-title", "Catch and Effort Data"),
        fluidRow(
          column(12,
            div(style = "margin-bottom: 15px;",
              actionButton("add_data", "Add data", class = "btn-sm btn-primary"),
              actionButton("edit_row", "Edit row", class = "btn-sm"),
              actionButton("delete_row", "Delete row", class = "btn-sm")
            )
          )
        ),
        fluidRow(
          column(6,
            div(style = "display: flex; align-items: center;",
              tags$label("Show", style = "margin-right: 10px;"),
              selectInput("show_entries", NULL, choices = c(10, 25, 50, 100), selected = 10, width = "80px"),
              tags$label("entries", style = "margin-left: 5px;")
            )
          ),
          column(6,
            div(style = "text-align: right;",
              tags$label("Search:", style = "margin-right: 10px;"),
              textInput("search", NULL, width = "200px")
            )
          )
        ),
        DTOutput("data_table"),
        div(style = "margin-top: 10px;", textOutput("table_info"))
      )
    } else if (page == "model") {
      div(class = "table-section",
        h2("Model Results"),
        p("Model results will be displayed here."),
        hr(),
        p("This section will show statistical analysis and modeling outputs based on your catch and effort data.")
      )
    } else if (page == "summary") {
      div(class = "table-section",
        h2("Summary"),
        p("Summary statistics and reports will be displayed here."),
        hr(),
        p("This section will provide an overview of your data and key metrics.")
      )
    }
  })
  
  output$data_table <- renderDT({
    datatable(fish_data(), 
    options = list(
      pageLength = as.numeric(input$show_entries), 
      searching = TRUE, 
      ordering = TRUE, 
      lengthChange = FALSE, 
      info = FALSE, 
      paging = TRUE, 
      scrollY = FALSE,
      scrollCollapse = FALSE), rownames = TRUE, selection = "single")
  }, server = FALSE)
  
  output$table_info <- renderText({
    total_entries <- nrow(fish_data())
    paste("Showing 1 to", min(as.numeric(input$show_entries), total_entries), "of", total_entries, "entries")
  })
  
  observeEvent(input$add_data, {
    showModal(modalDialog(
      title = "Add data",
      div(class = "modal-section",
        h4("Species"),
        selectInput("add_species", NULL, choices = c("Yellowfin Tuna", "Mahi Mahi", "Opakapaka", "Peacock Grouper", "Yellowfin Goatfish"), selected = "Yellowfin Tuna", width = "100%")
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
      div(class = "modal-section",
        h4("Total Landings"),
        numericInput("add_landings", NULL, value = 1, width = "100%")
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
      Date = input$add_date,
      Count = input$add_count,
      Effort = input$add_effort
      #Total_Landings = input$add_landings
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
        selectInput("edit_species", NULL, choices = c("Yellowfin Tuna", "Mahi Mahi", "Opakapaka", "Peacock Grouper", "Yellowfin Goatfish"), selected = row_data$Species, width = "100%")
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
        h4("Effort (hours)"),
        numericInput("edit_effort", NULL, value = row_data$Effort, min = 0, step = 0.5, width = "100%")
      ),
      # div(class = "modal-section",
      #   h4("Total Landings (lbs)"),
      #   numericInput("edit_landings", NULL, value = row_data$Total_Landings, min = 0, width = "100%")
      # ),
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
    
    current_data[selected, "Species"] <- input$edit_species
    current_data[selected, "Date"] <- input$edit_date
    current_data[selected, "Count"] <- input$edit_count
    current_data[selected, "Effort"] <- input$edit_effort
    #current_data[selected, "Total_Landings"] <- input$edit_landings
    
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
