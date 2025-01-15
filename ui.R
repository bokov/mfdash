library(shiny)
library(DT)


shinyUI(fluidPage(
  titlePanel("Market Data Viewer"),
  tags$script(HTML(
    "$(document).on('click', '.action-button', function() {
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
  fluidRow(
    actionButton('debug','Debug'),
    column(6, DTOutput("market_table")),
    column(6, DTOutput("details_table"))
  )
))