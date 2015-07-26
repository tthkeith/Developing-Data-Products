# The user-interface definition of the Shiny web app.
library(shiny)
library(BH)
library(rCharts)
require(markdown)
require(data.table)
library(dplyr)
library(DT)
shinyUI(
  navbarPage("State Employee Salary Visualizer",
             # multi-page user-interface that includes a navigation bar.
             tabPanel("Explore the Data",
                      sidebarPanel(
                        h4('Click on Clear Selection, Then Select All to view the data', align = "center"),
                        sliderInput("timeline",
                                    "Timeline:",
                                    min = 2012,
                                    max = 2014,
                                    value = c(2013, 2014)),
                        sliderInput("salary",
                                    "Annual Salaries:",
                                    min = 20000,
                                    max = 290000,
                                    value = c(50000, 100000)
                        ),
                        #format = "####"),
                        uiOutput("themesControl"), # the id
                        actionButton(inputId = "clearAll",
                                     label = "Clear selection",
                                     icon = icon("square-o")),
                        actionButton(inputId = "selectAll",
                                     label = "Select all",
                                     icon = icon("check-square-o"))
                      ),
                      mainPanel(
                        tabsetPanel(
                          # Data
                          tabPanel(p(icon("table"), "Dataset"),
                                   dataTableOutput(outputId="dTable")
                          ), # end of "Dataset" tab panel
                          tabPanel(p(icon("line-chart"), "Visualize the Data"),
                                   h4('Number People Surveyed by Year', align = "center"),
                                   showOutput("ServiceTypeByYear", "nvd3"),
                                   h4('Annual Salaries by Year', align = "center"),
                                   showOutput("SalaryByYear", "nvd3"),
                                   h4('Number of Average Salaries by Year', align = "center"),
                                   showOutput("SalaryByYearAvg", "nvd3"),
                                   h4('Number of Average Salaries by Classification', align = "center"),
                                   showOutput("SalaryByServiceTypeAvg", "nvd3")
                          ) # end of "Visualize the Data" tab panel
                        )
                      )
             ), # end of "Explore Dataset" tab panel
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             ) # end of "About" tab panel
  )
)