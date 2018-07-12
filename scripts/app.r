library(shiny)
library(shinythemes)
library(tidyverse)
library(bayesboot)
library(DT)
library(ggiraph)
source('fun_percentile.r')

cp = read_csv('../csv/Coastal_plain_cleaned.csv')

ui = fluidPage(
  title = 'Coastal Plain Stressor Analyis',
  tabsetPanel(type = 'tabs',
              #tabPanel('Plot', plotOutput('plot')),
              tabPanel('Plot', ggiraphOutput('plot2', height = 500)),
              tabPanel('Table', tableOutput('data_table')),
              tabPanel('Raw Data', dataTableOutput('raw_data'))
              ),
  hr(),
  fluidRow(
    column(3,
           uiOutput('choose_columns')
           ),
    column(3,
           uiOutput('conf_interval'),
           uiOutput('percentile')
           ),
    column(3,
           uiOutput('iterations'),
           actionButton('go', 'Calculate Percentile')
    )
  )
)

server = function(input, output, session){
  output$choose_columns = renderUI(
    selectInput('stressor', 'Stressor', 
                list(
                  'Physical Stressors' = c('Channel Modification' = 'CM', 
                                           'Instream Habitat' = 'IH',
                                           'Pools' = 'P',
                                           'Bank Stability Left' = 'BSL',
                                           'Bank Stability Right' = 'BSR',
                                           'Bank Vegetative Left' = 'BVL',
                                           'Bank Vegetative Right' = 'BVR',
                                           'Shading' = 'S',
                                           'Riparian Zone Left' = 'RZL',
                                           'Riparian Zone Right' = 'RZR'),
                  'Chemical Stressors' = c('Alkalinity', 'Ammonia', 'BOD20', 'BOD5', 
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
                                           'Water Temperature' = 'Temp', 'Turbidity')))
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
  
  output$conf_interval = renderUI(
    sliderInput('conf_interval', 'Confidence Interval',
                min = 0.6,
                value = 0.6,
                max = 1,
                step = 0.05)
  )
  
  observeEvent(input$go, {
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
    
    upper_conf = input$conf_interval
    r = as.numeric(input$interations)
    
    # Create progress bar
    progress = Progress$new(session, min = 1, max = 15)
    on.exit(progress$close())
    progress$set(message = 'Calculation in Progress')
    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    dat = pairwisePercentileTest(data = cp,
                                 group = 'Biotic.Classification',
                                 column = column,
                                 tau = tau,
                                 r = r)
    
    boot.cp = bootanalysis(data = cp, column = column, r = r, upper_conf = upper_conf)
    
    table = dat %>% 
      select(Comparison, p.adjust) %>% 
      mutate('Significant?' = ifelse(p.adjust <= 0.001, '***', 
                                     ifelse(p.adjust <= 0.01, '**', 
                                            ifelse(p.adjust <= 0.05, '*', ''))))
      #as_tibble()
    cp_plot = boot.cp %>% 
      ggplot(aes(y = Condition, x = Mean, color = Condition, tooltip = Mean), size = 2) +
      geom_point(aes(size = 2)) +
      geom_errorbarh(aes(xmax = Upper, xmin = Lower, y = Condition), height = 0.25, size = 1.25) + 
      theme(axis.title = element_blank(), legend.position = 'none') +
      xlab(label = element_blank()) +
      scale_y_discrete(limits = unique(rev(boot.cp$Condition)))
    
    g = ggplot(boot.cp, aes(y = Condition, x = Mean, color = Condition))
    
    my_gg = g + geom_point_interactive(aes(tooltip = Mean), size = 2) +
      geom_errorbarh(aes(xmax = Upper, xmin = Lower, height = 0.25)) +
      theme_tufte() + theme(axis.title = element_blank(), legend.position = 'none') +
      xlab(label = element_blank()) +
      scale_y_discrete(limits = unique(rev(boot.cp$Condition)))

    output$data_table = renderTable({boot.cp})
    #output$plot = renderPlot(cp_plot)
    output$plot2 = renderggiraph({ggiraph(code = print(my_gg))})
    output$raw_data = renderDataTable(datatable(cp, options = list(lengthMenu = list(c(5, 15, 25, -1), c('5', '15', '25', 'All')),
                                                                   pageLength = 15
                                                                     )))
    
    })
}

shinyApp(ui, server)