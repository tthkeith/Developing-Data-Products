library(shiny)
# Load data processing file
source("data_processing.R")
serviceTypes <- sort(unique(data$SERVICE.TYPE))
# Shiny server
shinyServer(
  function(input, output) {
    # Initialize reactive values
    values <- reactiveValues()
    values$serviceTypes <- serviceTypes
    # Create event type checkbox
    output$themesControl <- renderUI({
      checkboxGroupInput('serviceTypes', 'Job Service Types:',
                         serviceTypes, selected = values$serviceTypes)
    })
    # Add observer on select-all button
    observe({
      if(input$selectAll == 0) return()
      values$serviceTypes <- serviceTypes
    })
    # Add observer on clear-all button
    observe({
      if(input$clearAll == 0) return()
      values$serviceTypes <- c() # empty list
    })
    # Prepare dataset
    dataTable <- reactive({
      groupByServiceType(data, input$timeline[1],
                         input$timeline[2], input$salary[1],
                         input$salary[2], input$serviceTypes)
    })
    dataTableByYear <- reactive({
      groupByYearAgg(data, input$timeline[1],
                     input$timeline[2], input$salary[1],
                     input$salary[2], input$serviceTypes)
    })
    dataTableBySalary <- reactive({
      groupByYearSalary(data, input$timeline[1],
                        input$timeline[2], input$salary[1],
                        input$salary[2], input$serviceTypes)
    })
    dataTableBySalaryAvg <- reactive({
      groupBySalaryAvg(data, input$timeline[1],
                       input$timeline[2], input$salary[1],
                       input$salary[2], input$serviceTypes)
    })
    dataTableBySalaryServiceTypeAvg <- reactive({
      groupBySalaryServiceTypeAvg(data, input$timeline[1],
                                  input$timeline[2], input$salary[1],
                                  input$salary[2], input$serviceTypes)
    })
    #Render data table
    output$dTable <- renderDataTable({
      dataTable()
    } 
    )
    output$ServiceTypeByYear <- renderChart({
      plotServiceTypeCountByYear(dataTableByYear())
    })
    output$SalaryByYear <- renderChart({
      plotSalaryByYear(dataTableBySalary())
    })
    output$SalaryByYearAvg <- renderChart({
      plotSalaryByYearAvg(dataTableBySalaryAvg())
    })
    output$SalaryByServiceTypeAvg <- renderChart({
      plotSalaryByServiceTypeAvg(dataTableBySalaryServiceTypeAvg())
    })
  } # end of function(input, output)
)