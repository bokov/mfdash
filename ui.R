library(shiny)
library(DT)

shinyUI(fluidPage(
  titlePanel("MV"),
  tags$script(HTML(
    "$(document).on('click', '.add-button', function() {
       var slug = this.id.replace('btn_', '');
       Shiny.setInputValue('slugToAdd', slug);
     });
     $(document).on('click', '.remove-button', function() {
       var detailID = this.id.replace('remove_', '');
       Shiny.setInputValue('detailToRemove', detailID);
     });
     $(document).on('click', '.plot-button', function() {
       var detailID = this.id.replace('plot_', '');
       Shiny.setInputValue('detailToPlot', detailID);
     });"
  )),
  navbarPage(
    "Market Data Viewer",
    header=list(if(file.exists('.debug')) actionButton('debug', 'Debug')),
    tabPanel(
      "Market Details",
      fluidRow(
        column(6, dataTableOutput("market_table")),
        column(6, DTOutput("details_table"))
      )
    ),
    tabPanel(
      "Bets",
      DTOutput("bets_table")
    )
  )
));