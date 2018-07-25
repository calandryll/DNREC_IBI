library(shiny)
library(shinythemes)
library(tidyverse)
library(bayesboot)
library(DT)
library(ggthemes)
source('fun_percentile.r')

combined = read_csv('../csv/IBI_cleaned.csv')

ui = fluidPage(
  theme = shinytheme('paper'),
  title = 'Delaware Stressor Analyis',
  tabsetPanel(type = 'tabs',
              tabPanel('Instructions', includeHTML('../Instruction.html')),
              tabPanel('Plot', plotOutput('plot')),
              tabPanel('Data Table', dataTableOutput('sig_table')),
              tabPanel('Ratio Analysis', dataTableOutput('ratio_table')),
              tabPanel('Raw Data', dataTableOutput('raw_data')),
              tabPanel('About', includeHTML('../About.html'))
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
           actionButton('go', 'Calculate')
    )
  )
)

server = function(input, output, session){
  output$choose_columns = renderUI(
    selectInput('stressor', 'Stressor', 
                list(
                  'Physical Stressors' = c('Channel Modification' = 'CM',
                                           'Bottom Substrate Cover' = 'BSC',
                                           'Embedness' = 'E',
                                           'Riffle Quality' = 'RQ',
                                           'Frequency of Riffles' = 'FR',
                                           'Sediment Deposition' = 'SD',
                                           'Velocity Depth' = 'VD',
                                           'Instream Habitat' = 'IH',
                                           'Pools' = 'P',
                                           'Bank Stability Left' = 'BSL',
                                           'Bank Stability Right' = 'BSR',
                                           'Bank Vegetative Left' = 'BVL',
                                           'Bank Vegetative Right' = 'BVR',
                                           'Shading' = 'S',
                                           'Riparian Zone Left' = 'RZL',
                                           'Riparian Zone Right' = 'RZR'),
                  'Chemical Stressors' = c('Alkalinity', 'Ammonia',
                                           'CBOD at 20 Day' = 'BOD20', 
                                           'CBOD at 5 Day' = 'BOD5', 
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
                                           'Total Phosphorus' = 'TP',
                                           'Total Suspended Solids' = 'TSS',
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
    
    ibi.combined = ratioanalysis(data = combined, 
                                 column = column, 
                                 r = r, 
                                 upper_conf = upper_conf, 
                                 tau = tau)
    
    combined_plot = ibi.combined$Boot %>% 
      ggplot(aes(y = Factor, x = Mean, shape = Condition), size = 6, color = 'black') +
      geom_errorbarh(aes(xmax = `Upper CI`, xmin = `Lower CI`, y = Factor), height = 0.25, size = 1.25) +
      geom_point(aes(color = Region, shape = Condition), size = 5.5) +
      xlab(label = column) +
      scale_y_discrete(limits = c('Severely Degraded_Coastal Plain', 
                                  'Severely Degraded_Piedmont', 
                                  'Moderately Degraded_Coastal Plain', 
                                  'Moderately Degraded_Piedmont', 
                                  'Good Condition_Coastal Plain', 
                                  'Good Condition_Piedmont')) + 
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line.x = element_line(color = 'black'),
            axis.line.y = element_line(color = 'black'),
            text = element_text(family = 'serif', size = 18),
            legend.key = element_blank(),
            axis.text.x = element_text(size = 16)
      )
    
    output$sig_table = renderDataTable(datatable(ibi.combined$Sig, 
                                                  options = list(paging = FALSE, searching = FALSE), 
                                                  rownames = FALSE))
    output$ratio_table = renderDataTable(datatable(ibi.combined$Ratio, 
                                                   options = list(paging = FALSE, searching = FALSE), 
                                                   rownames = FALSE))
    output$plot = renderPlot(combined_plot)
    
  })
  
  cleaned_combined = combined %>% 
    select(Region, Site, Watershed, 'Biotic Classification' = Biotic.Classification, CM:Turbidity)
  
  output$raw_data = renderDataTable(datatable(cleaned_combined, 
                                              options = list(pageLength = 10, dom = 'ftip'), 
                                              rownames = FALSE))
}

shinyApp(ui, server)