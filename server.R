library(shiny)
library(DT)
library(dplyr)

shinyServer(function(input, output, session) {
  rv <- reactiveValues(details = NULL)
  
  # Prepare datatable with buttons
  output$market_table <- renderDT({
    market_data %>% 
      transmute(
        closeTime #, question, url
        , outcomeType, uniqueBettorCount
        , lastUpdatedTime, probability
        , question = sprintf('<a href="%s" target="_blank">%s</a>',url,question)
        ,action = sprintf('<button class="btn btn-success btn-sm action-button" id="btn_%s">
                          <span>&#9654;</span></button>',slug)) %>% 
#      select(closeTime, question, url, outcomeType, uniqueBettorCount, lastUpdatedTime, probability, action) %>% 
      datatable(escape = FALSE
                , options = list(scrollY = "400px", paging = FALSE)
                , rownames='',selection='none')
  }, server = TRUE)
  
  # Prepare details datatable
  output$details_table <- renderDT({
    if (is.null(rv$details) || nrow(rv$details) == 0) {
      return(datatable(data.frame(Message = "Select some markets")
                       , options = list(dom = 't'),rownames = ''))
    }
    rv$details %>% 
      mutate(
        remove = paste0(
          '<button class="btn btn-danger btn-sm remove-button" id="remove_', id, '">',
          '<span>&#10006;</span></button>'
        ),
        plot = paste0(
          '<button class="btn btn-success btn-sm plot-button" id="plot_', id, '">',
          '<span>&#128200;</span></button>'
        )
      ) %>% 
      datatable(escape = FALSE, options = list(scrollY = "400px", paging = FALSE)
                ,rownames = '', selection='none')
  })
  
  # Handle custom input 'slugToAdd'
  observeEvent(input$slugToAdd, {
    slug <- input$slugToAdd
    slugDetails <- try(expandDetails(slug), silent = TRUE)
    if (!inherits(slugDetails, 'try-error')) {
      rv$details <- bind_rows(rv$details, slugDetails) %>% distinct()
    }
  })
  
  # Handle removal of details
  observeEvent(input$detailToRemove, {
    detailID <- input$detailToRemove;
    message('remove requested: ',detailID);
    #rv$details <- rv$details %>% filter(detailID != detailID)
  })
  
  # Handle plotting of details
  observeEvent(input$detailToPlot, {
    detailID <- input$detailToPlot;
    message(paste("Plotting detail ID:", detailID))
    # Add your plotting logic here
  })
  
  observeEvent(input$debug,browser());
})