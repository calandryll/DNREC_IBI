library(shiny)
library(tidyverse)
source('fun_percentile.r')

cp = read_csv('../csv/Coastal_plain_cleaned.csv')

ui = fluidPage(
  titlePanel('Pairwise Percentile Analysis'),
  sidebarPanel(
    helpText('Select factor, number of iterations and percentile'),
    uiOutput('choose_columns'),
    uiOutput('iterations'),
    uiOutput('percentile'),
    actionButton('go', 'Calculate Percentile')
  ),
  mainPanel(verbatimTextOutput('data_table')
  )
)

server = function(input, output){
  # Get Column names
  colnames = cp %>% select(-Site, -Watershed, -Biotic.Classification) %>% names()

  output$choose_columns = renderUI(
    selectInput('stressor', 'Stressor', 
                choices = colnames)
  )
  
  output$iterations = renderUI(
    numericInput('interations', 'Iterations', 
                 min = 1000, 
                 max = 100000, 
                 step = 1000, 
                 value = 1000)
  )
  
  output$percentile = renderUI(
    selectInput('percentile', 'Percentile',
                choices = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
                )
  )
  
  
  observeEvent(input$go, {
    #test = cp %>% select(Site, Watershed, Biotic.Classification, input$stressor)
    #output$data_table = renderTable(head(test, 20))
    column = as.character(input$stressor)
    tau = switch(input$percentile,
                 '0.1' = 0.1,
                 '0.2' = 0.2,
                 '0.3' = 0.3,
                 '0.4' = 0.4,
                 '0.5' = 0.5,
                 '0.6' = 0.6,
                 '0.7' = 0.7,
                 '0.8' = 0.8,
                 '0.9' = 0.9
    )
    r = as.numeric(input$interations)
    dat = pairwisePercentileTest(data = cp,
                                 group = 'Biotic.Classification',
                                 column = column,
                                 tau = tau,
                                 r = r)
    
    table = dat %>% select(Comparison, p.adjust) %>% mutate(Sig = ifelse(p.adjust <= 0.001, '***', ifelse(p.adjust <= 0.01, '**', ifelse(p.adjust <= 0.05, '*', ''))))
 
    output$data_table = renderPrint({column
      tau
      table})
    })

}

shinyApp(ui = ui, server = server)