library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggthemes)
library(knitr)
library(kableExtra)
source('fun_ratio.r')

combined = read_csv('../csv/IBI_cleaned.csv')

ui = fluidPage(
  theme = shinytheme('paper'),
  title = 'Delaware Stressor Analyis',
  tabsetPanel(type = 'tabs',
              tabPanel('Instructions', includeHTML('../Instruction2.html')),
              tabPanel('Ratio Analysis', dataTableOutput('ratio_table'), tableOutput('cp_ratio'), tableOutput('pied_ratio'))
  ),
  hr(),
  fluidRow(
    column(3,
           uiOutput('choose_columns'),
           numericInput('threshold', 'Threshold:', 1),
           uiOutput('percentile')
    ),
    column(3,
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
  

  output$percentile = renderUI(
    selectInput('percentile', 'Percentile',
                choices = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    )
  )
  
  output$threshold = renderUI(
    renderPrint({input$threshold})
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
    
    

    threshold = as.numeric(input$threshold)

    
    ibi.combined = ratio_analysis(data = combined,
                                  column = column,
                                  threshold = threshold, 
                                  tau = tau)
    
    if(!all(is.na(ibi.combined$Ratio$`Odds Ratio`))){
      output$ratio_table = renderDataTable(datatable(ibi.combined$Ratio, 
                                                     options = list(paging = FALSE, searching = FALSE), 
                                                     rownames = FALSE) %>%
                                             formatRound(columns = c('Odds Ratio', 
                                                                     'Risk Cases', 'Risk Controls', 
                                                                     'Attributable Risk'), digits = 3))
    }
    if(!all(is.na(ibi.combined$CP_Ratio_Table))){
      output$cp_ratio = renderText(kable(ibi.combined$CP_Ratio_Table, row.names = TRUE, 
                                         align = c('c', 'c', 'c'), caption = 'Coastal Plain Contingency Table',
                                         format = 'html') %>% 
                                     kable_styling(bootstrap_options = c('striped', 'hover', 'responsive')))
    }
    if(!all(is.na(ibi.combined$Pied_Ratio_Table))){
      output$pied_ratio = renderText(kable(ibi.combined$Pied_Ratio_Table, row.names = TRUE, 
                                           align = c('c', 'c', 'c'), caption = 'Piedmont Contingency Table',
                                           format = 'html') %>% 
                                       kable_styling(bootstrap_options = c('striped', 'hover', 'responsive')))
    }

  })
  
 
}

shinyApp(ui, server)