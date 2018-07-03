library(shiny)
library(tidyverse)
library(ggthemes)
source('fun_percentile.r')

cp = read_csv('../csv/Coastal_plain_cleaned.csv')
cp_plot = read_csv('../csv/Coastal_plain_bootstrap.csv')

ui = fluidPage(
  titlePanel('Pairwise Percentile Analysis'),
  sidebarPanel(
    helpText('Select factor, number of iterations and percentile'),
    uiOutput('choose_columns'),
    uiOutput('iterations'),
    uiOutput('percentile'),
    actionButton('go', 'Calculate Percentile')
  ),
  mainPanel(plotOutput('plot'),
    verbatimTextOutput('data_table')
  )
)

server = function(input, output, session){
  
  output$choose_columns = renderUI(
    selectInput('stressor', 'Stressor', 
                choices = c('Channel Modification' = 'CM', 
                            'Instream Habitat' = 'IH',
                            'Pools' = 'P',
                            'Bank Stability Left' = 'BSL',
                            'Bank Stability Right' = 'BSR',
                            'Bank Vegetative Left' = 'BVL',
                            'Bank Vegetative Right' = 'BVR',
                            'Shading' = 'S',
                            'Riparian Zone Left' = 'RZL',
                            'Riparian Zone Right' = 'RZR', 
                            'Alkalinity', 'Ammonia', 'BOD20', 'BOD5', 
                            'Dissolved Organic Carbon' = 'DOC',
                            'Total Organic Carbon' = 'TOC', 'Chloride',
                            'Chlorophyll a' = 'Chlorophyll_a',
                            'Specific Conductivity' = 'SpCond',
                            'Flow' = 'cfs', 'Hardness',
                            'Nitrate/Nitrite' = 'NOx',
                            'Total Nitrogen' = 'TN',
                            'Orthophosphate' = 'OP',
                            'Dissolved Oxygen' = 'DO',
                            'Dissolved Oxygen Saturation' = 'DOSat', 'pH',
                            'Total Phosphorus' = 'TP', 'TSS',
                            'Salinity',
                            'Air Temperature' = 'ATemp',
                            'Water Temperature' = 'Temp', 'Turbidity'))
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

    table = dat %>% select(Comparison, p.adjust) %>% mutate('Significant?' = ifelse(p.adjust <= 0.001, '***', ifelse(p.adjust <= 0.01, '**', ifelse(p.adjust <= 0.05, '*', ''))))
    output$plot = renderPlot({
      cp_plot %>% 
        filter(Test == column) %>% 
        spread(Stat, Data) %>% 
        ggplot(aes(y = Condition, x = Mean, color = Condition)) + 
        geom_point(aes(size = 1)) + 
        geom_segment(aes(xend = Upper, x = Lower, y = Condition, yend = Condition)) + 
        theme(axis.title = element_blank(), legend.position = 'none') +
        scale_y_discrete(limits = unique(rev(cp_plot$Condition)))
    })
    output$data_table = renderPrint({progress = Progress$new(session, min = 1, max = 15)
    on.exit(progress$close())
    progress$set(message = 'Calculation in Progress')
    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    table})
    })

}

shinyApp(ui = ui, server = server)