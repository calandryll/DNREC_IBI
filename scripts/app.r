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
  title = 'Coastal Plain Stressor Analyis',
  tabsetPanel(type = 'tabs',
              tabPanel('Plot', plotOutput('plot')),
              tabPanel('Table', dataTableOutput('data_table')),
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
    cp = combined %>% filter(Region == 'Coastal Plain')
    dat.cp = pairwisePercentileTest(data = cp,
                                 group = 'Biotic.Classification',
                                 column = column,
                                 tau = tau,
                                 r = r)
    
    pied = combined %>% filter(Region == 'Piedmont')
    dat.pied = pairwisePercentileTest(data = pied,
                                    group = 'Biotic.Classification',
                                    column = column,
                                    tau = tau,
                                    r = r)
    
    boot.combined = bootanalysis(data = combined, column = column, r = r, upper_conf = upper_conf) %>% unite(Factor, Condition, Region, remove = FALSE)
    
    table.cp = dat.cp %>% 
      select(Comparison, p.adjust) %>% 
      mutate('Significant?' = ifelse(p.adjust <= 0.001, '***', 
                                     ifelse(p.adjust <= 0.01, '**', 
                                            ifelse(p.adjust <= 0.05, '*', NA))), Region = 'Coastal Plain') %>%
      as_tibble()
    
    table.pied = dat.pied %>% 
      select(Comparison, p.adjust) %>% 
      mutate('Significant?' = ifelse(p.adjust <= 0.001, '***', 
                                     ifelse(p.adjust <= 0.01, '**', 
                                            ifelse(p.adjust <= 0.05, '*', NA))), Region = 'Piedmont') %>%
      as_tibble()
    
    table = bind_rows(table.cp, table.pied)
    
    cp_plot = boot.combined %>% 
      filter(Region == 'Coastal Plain') %>%
      ggplot(aes(y = Condition, x = Mean, color = Condition, size = 2)) +
      geom_point(aes(size = 2)) +
      geom_errorbarh(aes(xmax = `Upper CI`, xmin = `Lower CI`, y = Condition), height = 0.25, size = 1.25) + 
      theme_tufte(base_size = 15) + 
      theme(axis.title = element_blank(), legend.position = 'none') +
      xlab(label = element_blank()) +
      scale_y_discrete(limits = unique(rev(boot.combined$Condition)))
    
    pied_plot = boot.combined %>% 
      filter(Region == 'Piedmont') %>%
      ggplot(aes(y = Condition, x = Mean, color = Condition, size = 2)) +
      geom_point(aes(size = 2)) +
      geom_errorbarh(aes(xmax = `Upper CI`, xmin = `Lower CI`, y = Condition), height = 0.25, size = 1.25) + 
      theme_tufte(base_size = 15) + 
      theme(axis.title = element_blank(), legend.position = 'none') +
      xlab(label = element_blank()) +
      scale_y_discrete(limits = unique(rev(boot.combined$Condition)))
    
    sig_table = bind_cols(boot.combined, table) %>% select(-Factor, -Region1)
    
    combined_plot = boot.combined %>% 
      ggplot(aes(y = Factor, x = Mean, color = Region, shape = Condition), size = 2) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmax = `Upper CI`, xmin = `Lower CI`, y = Factor), height = 0.25, size = 1.25) + 
      theme_tufte(base_size = 15) + 
      xlab(label = element_blank()) +
      scale_y_discrete(limits = c('Severely Degraded_Coastal Plain', 'Severely Degraded_Piedmont', 'Moderately Degraded_Coastal Plain', 'Moderately Degraded_Piedmont', 'Good Condition_Coastal Plain', 'Good Condition_Piedmont')) + 
      theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    output$data_table = renderDataTable(datatable(sig_table, options = list(paging = FALSE, searching = FALSE)))
    output$plot = renderPlot(combined_plot)
    #output$plot2 = renderPlot(pied_plot)
    output$raw_data = renderDataTable(datatable(cp, options = list(lengthMenu = list(c(5, 15, 25, -1), c('5', '15', '25', 'All')),
                                                                   pageLength = 15
                                                                     )))
    
    })
}

shinyApp(ui, server)