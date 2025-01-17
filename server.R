library(shiny)
library(DT)
library(dplyr)

shinyServer(function(input, output, session) {
  rv <- reactiveValues(details = NULL)
  
  # Prepare datatable with buttons
  output$market_table <- renderDataTable(market_data_for_DT, server = TRUE);
  
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
      datatable(escape = FALSE
                , options = list(scrollY = "400px", scrollX= T, scrollCollapse=T
                                 , fixedHeader=T, paging = FALSE)
                ,rownames = '', selection='none')
  });
  
  output$bets_table <- renderDT({
    if(is.null(rv$prices)||nrow(rv$prices)==0){
      return(datatable(data.frame(Message='Select some markets or answers'
                                  ,options=list(dom='t'),rownames = '')))
    } else {
      rv$prices %>% transmute(contractId,question,answerId,text) %>% unique() %>%
        datatable(options=list(scrollY='400px',paging=F,rownames=''));
    };
  });
  
  # Handle custom input 'slugToAdd'
  observeEvent(input$slugToAdd, {
    slug <- input$slugToAdd;
    if(is.null(rv$details) || !slug %in% rv$details$slug){
      # TODO simply check for slug already existing in rv$details and skip if
      #      it does or if rv$details is null
      slugDetails <- try(expandDetails(slug), silent = TRUE);
      if (!inherits(slugDetails, 'try-error')) {
        # insert dummy values for numeric markets
        if(!hasName(slugDetails,'id.answer')) slugDetails$id.answer <- '';
        if(!hasName(slugDetails,'text')) slugDetails$text <- '';
        # update the details reactive value 
        rv$details <- bind_rows(rv$details, slugDetails) %>% distinct();
        message("Downloaded details for:",slug);
      } else {
        message("slug ... ",slugDetails);
        browser();
        };
    } else message("Details already cached for:",slug);
  });
  
  # Handle removal of details
  observeEvent(input$detailToRemove, {
    detailID <- input$detailToRemove;
    message('remove requested: ',detailID);
    #rv$details <- rv$details %>% filter(detailID != detailID)
  })
  
  # Handle plotting of details
  observeEvent(input$detailToPlot, {
    pricesID <- input$detailToPlot;
    if(is.null(rv$prices)||!pricesID %in% rv$prices$contractId){
      newPrices <- try(manifold_api(endpoint='/v0/bets',request_type='GET'
                                ,params_list=list(contractId=pricesID
                                                  ,includeZeroShareRedemptions='true'
                                                  ,filterRedemptions='false'))$content %>% 
        lapply(flattenmarketlistdata) %>% bind_rows());
      if(!inherits(newPrices,'try-error')){
        # insert dummy values for numeric markets
        if(!hasName(newPrices,'answerId')) newPrices$answerId <- '';
        newPrices <- left_join(newPrices
                               ,rv$details[,c('id.answer','id','question','text')]
                               ,by=c(answerId='id.answer',contractId='id')
                               ,suffix=c('.bet','.details'));
        rv$prices <- bind_rows(rv$prices, newPrices) %>% distinct();
        message("Downloaded prices for ID:", pricesID);
      } else message(newPrices);
    } else message("Prices already cached for ID:", pricesID);
    # Add your plotting logic here
  })
  
  observeEvent(input$debug,browser());
  
})