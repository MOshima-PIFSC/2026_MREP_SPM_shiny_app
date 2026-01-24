
server <- function(input, output, session) {
  
  penguin_data <- data.frame(
    species = rep("Adelie", 10),
    island = rep("Torgersen", 10),
    bill_length_mm = c(39.1, 39.5, 40.3, 40, 36.7, 39.3, 38.9, 39.2, 34.1, 42),
    body_mass_g = c(3750, 3800, 3250, 3925, 3450, 3650, 3625, 4675, 3475, 4250)
  )
  
  output$data_table <- renderDT({
    datatable(penguin_data,
              options = list(
                pageLength = as.numeric(input$show_entries),
                searching = TRUE,
                ordering = TRUE,
                lengthChange = FALSE,
                info = FALSE,
                paging = TRUE
              ),
              rownames = TRUE,
              selection = "single"
    )
  }, server = FALSE)
  
  output$table_info <- renderText({
    total_entries <- nrow(penguin_data)
    paste("Showing 1 to", min(as.numeric(input$show_entries), total_entries), 
          "of", total_entries, "entries")
  })
  
  observeEvent(input$add_data, {
    showModal(modalDialog(
      title = "Add data",
      
      div(class = "modal-section",
        h4("Radio buttons"),
        radioButtons("modal_radio", NULL,
                    choices = c("Choice 1", "Choice 2", "Choice 3"),
                    selected = "Choice 1")
      ),
      
      div(class = "modal-section",
        h4("Numeric input"),
        numericInput("modal_numeric1", NULL, value = 1, width = "100%")
      ),
      
      div(class = "modal-section",
        h4("Numeric input"),
        numericInput("modal_numeric2", NULL, value = 1, width = "100%")
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_data", "Action", class = "btn-primary")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$submit_data, {
    showNotification("Data submitted!", type = "message")
    removeModal()
  })
  
  observeEvent(input$edit_row, {
    showModal(modalDialog(
      title = "Edit Row",
      "Edit row functionality would go here",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$delete_row, {
    showModal(modalDialog(
      title = "Delete Row",
      "Delete row functionality would go here",
      easyClose = TRUE
    ))
  })
}
