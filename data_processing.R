# Load required libraries
#library(sqldf)
require(data.table)
library(dplyr)
library(DT)
library(rCharts)

# Data Loading

## Read data
data1 <- data.frame(fread("./data/Salaries__State_Agencies__As_of_June_30__2012.csv"))
data2 <- data.frame(fread("./data/Salaries__State_Agencies__As_of_June_30__2013.csv"))
data3 <- data.frame(fread("./data/Salaries__State_Agencies__As_of_June_30__2014.csv"))

###Set column names
cols <-c('AGENCY #','AGENCY TITLE','CLASSIFICATION','SERVICE TYPE','FULL/PART TIME','ANNUAL SALARY','FISCAL YEAR')

###Rename the columns
colnames(data1) <- cols
colnames(data2) <- cols
colnames(data3) <- cols

###Combine the datasets
combined_data <- data.frame(rbind(data1, data2,data3))

cols2 <-c('AGENCY','AGENCY.TITLE','CLASSIFICATION','SERVICE.TYPE','FT.PT','ANNUAL.SALARY','FISCAL.YEAR')

colnames(combined_data) <- cols2

###Convert columns to the right data type
combined_data$AGENCY <- sapply(combined_data$AGENCY, function(x) as.integer(as.character(x))) 
combined_data$ANNUAL.SALARY <- sapply(combined_data$ANNUAL.SALARY, function(x) as.double(as.character(gsub("\\$","",x)))) 
combined_data$FISCAL.YEAR <- sapply(combined_data$FISCAL.YEAR, function(x) as.integer(as.character(x))) 

colnames(combined_data) <- cols2
## Data Preprocessing 
#Remove rows that contains NAs
data <- na.omit(combined_data)

data <- data[sample(nrow(data), 3000), ]

# Exploratory data analysis

sum(is.na(data)) # 0

length(unique(data$AGENCY.TITLE))
length(table(data$FISCAL.YEAR)) 
length(table(data$SERVICE.TYPE)) 

#Plotting Dimensions
serviceTypes <- sort(unique(data$SERVICE.TYPE))
agencyTitles <- sort(unique(data$AGENCY.TITLE))
years <- sort(unique(data$FISCAL.YEAR))


# sqldf("SELECT distinct FISCAL.YEAR FROM data")
## Helper functions
#' Aggregate dataset by FISCAL.YEAR
#'
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minSalary
#' @param maxSalary
#' @param serviceTypes
#' @return data.table
#'
groupByYearSalary <- function(dt, minYear, maxYear, minSalary,
                              maxSalary, serviceTypes) {
  result <- dt %>% filter(FISCAL.YEAR >= minYear, FISCAL.YEAR <= maxYear,ANNUAL.SALARY >= minSalary, ANNUAL.SALARY <= maxSalary,SERVICE.TYPE  %in% serviceTypes)
  return(result)
}
#' Aggregate dataset by serviceType
#'
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minSalary
#' @param maxSalary
#' @param serviceTypes
#' @return result data.table
#'
groupByServiceType <- function(dt, minYear, maxYear,
                               minSalary, maxSalary, serviceTypes) {
  # use pipelining
  dt <- groupByYearSalary(dt, minYear, maxYear, minSalary,
                          maxSalary, serviceTypes)
  result <- datatable(dt, options = list(iDisplayLength = 50))
  return(result)
  
}
#' Aggregate dataset by year to get total count of serviceType
#'
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minSalary
#' @param maxSalary
#' @param serviceTypes
#' @return data.table 2 columns
#'
groupByYearAgg <- function(dt, minYear, maxYear, minSalary,
                           maxSalary, serviceTypes) {
  dt <- groupByYearSalary(dt, minYear, maxYear, minSalary,
                          maxSalary, serviceTypes)
  result <- dt %>%
    group_by(FISCAL.YEAR) %>%
    summarise(count = n()) %>%
    arrange(FISCAL.YEAR)
  return(result)
}
#' Aggregate dataset by year to get total count of average
#' Annual Salaries
#'
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minSalary
#' @param maxSalary
#' @param serviceTypes
#' @return data.table 2 columns
#'
groupBySalaryAvg <- function(dt, minYear, maxYear, minSalary,
                             maxSalary, serviceTypes) {
  dt <- groupByYearSalary(dt, minYear, maxYear, minSalary,
                          maxSalary, serviceTypes)
  result <- dt %>%
    group_by(FISCAL.YEAR) %>%
    summarise(avg = mean(ANNUAL.SALARY)) %>%
    arrange(FISCAL.YEAR)
  return(result)
}
#' Average salary for each service type
#'
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minSalary
#' @param maxSalary
#' @param serviceTypes
#' @return data.table 2 columns
#'
groupBySalaryServiceTypeAvg <- function(dt, minYear, maxYear, minSalary,
                                        maxSalary, serviceTypes) {
  dt <- groupByYearSalary(dt, minYear, maxYear, minSalary,
                          maxSalary, serviceTypes)
  result <- dt %>%
    group_by(SERVICE.TYPE) %>%
    summarise(avgSalary = mean(ANNUAL.SALARY)) %>%
    arrange(SERVICE.TYPE)
  return(result)
}
#' Plot number of Service Type by year
#'
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of service type
#' @return ServiceTypeByYear plot
plotServiceTypeCountByYear <- function(dt, dom = "ServiceTypeByYear",
                                       xAxisLabel = "Fiscal Year",
                                       yAxisLabel = "Number of Service Types") {
  ServiceTypeByYear <- nPlot(
    count ~ FISCAL.YEAR,
    data = dt,
    #type = "lineChart",
    type = "multiBarChart",
    dom = dom, width = 650
  )
  ServiceTypeByYear$chart(margin = list(left = 100, tick=unique(dt$FISCAL.YEAR)))
  ServiceTypeByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  ServiceTypeByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  ServiceTypeByYear
}
#' Plot number of Service Type by year
#'
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel number of Service Type
#' @return SalaryByYear plot
plotSalaryByYear <- function(dt, dom = "SalaryByYear",
                             xAxisLabel = "Fiscal Year",
                             yAxisLabel = "Annual Salary") {
  SalaryByYear <- nPlot(
    ANNUAL.SALARY ~ FISCAL.YEAR,
    data = dt,
    # group = "year",
    type = "scatterChart",
    dom = dom, width = 650
  )
  SalaryByYear$chart(margin = list(left = 100,tick=unique(dt$FISCAL.YEAR)),
                     showDistX = TRUE,
                     showDistY = TRUE)
  SalaryByYear$chart(color = c('green', 'orange', 'blue'))
  #  SalaryByYear$chart(tooltipContent = "#! function(key, x, y, e){
  #                     return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
  #                     + '<b>Set AGENCY TILE</b>: ' + e.point.setId
  #                     + '</h5>'
  #} !#") # data[data$ANNUAL.SALARY==y&data$FISCAL.YEAR==x, ]$AGENCY.TITLE
  SalaryByYear$yAxis(axisLabel = yAxisLabel, width = 80)
  SalaryByYear$xAxis(axisLabel = xAxisLabel, width = 70)
  # piecesByYear$chart(useInteractiveGuideline = TRUE)
  SalaryByYear
}
#' Plot number of average salaries by year
#'
#' @param dt data.table
#' @param dom
#' @param xAxisLabel year
#' @param yAxisLabel Annual Salary
#' @return plotSalaryByYearAvg plot
plotSalaryByYearAvg <- function(dt, dom = "SalaryByYearAvg",
                                xAxisLabel = "Fiscal Year",
                                yAxisLabel = "Annual Salaries") {
  SalaryByYearAvg <- nPlot(
    avg ~ FISCAL.YEAR,
    data = dt,
    type = "lineChart",
    dom = dom, width = 650
  )
  SalaryByYearAvg$chart(margin = list(left = 100,tick=unique(dt$FISCAL.YEAR)))
  SalaryByYearAvg$chart(color = c('orange', 'blue', 'green'))
  SalaryByYearAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  SalaryByYearAvg$xAxis(axisLabel = xAxisLabel, width = 70)
  return(SalaryByYearAvg)
}
#' Plot number of average salary by service Type
#'
#' @param dt data.table
#' @param dom
#' @param xAxisLabel service type
#' @param yAxisLabel salary amount
#' @return plotSalaryByServiceTypeAvg plot
plotSalaryByServiceTypeAvg <- function(dt, dom = "SalaryByServiceTypeAvg",
                                       xAxisLabel = "Service Type",
                                       yAxisLabel = "Annual Salary") {
  SalaryByServiceTypeAvg <- nPlot(
    avgSalary ~ SERVICE.TYPE,
    data = dt,
    type = "multiBarChart",
    dom = dom, width = 650
  )
  SalaryByServiceTypeAvg$chart(margin = list(left = 100,tick=unique(dt$FISCAL.YEAR)))
  SalaryByServiceTypeAvg$chart(color = c('pink', 'blue', 'green'))
  SalaryByServiceTypeAvg$yAxis(axisLabel = yAxisLabel, width = 80)
  SalaryByServiceTypeAvg$xAxis(axisLabel = xAxisLabel, width = 200,
                               rotateLabels = -20, height = 200)
  return(SalaryByServiceTypeAvg)
}