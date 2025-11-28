## ---------------------------
##
## Course: 732A94-Advanced R Programming
##
## Script name: LAB 05
##
## Date Created: 2023-10-06
##
## Copyright (c) MIT
## ---------------------------

library(shiny)
library(bslib)
library(shinydashboard)
library(DT)

library(ggplot2)


# Define UI for application that draws the word cloud
shinyUI(

    dashboardPage(
     dashboardHeader(title = "Kolada Data Viewer"),
     dashboardSidebar(
       sidebarMenu(
         menuItem("Municipality Info", icon = icon("th"), tabName = "municipality_info"),
         menuItem("Municipality KPI Groups", icon = icon("th"), tabName = "municipality_kpi_groups"),
         menuItem("Municipality KPI", icon = icon("th"), tabName = "municipality_kpi"),
         menuItem("Demo Report", icon = icon("chart-line"), tabName = "reports")
       )
     ),
   dashboardBody(
       tabItems(
         # Municipality Info tab content
         tabItem(tabName = "municipality_info",
                   
                 fluidRow(
                      layout_columns(
                          fill = FALSE,
                          textInput("search_municipality_title_1","KPI Category",width="100%",placeholder = "KPI Category name, please input Befolkning")
                        ),
                     dataTableOutput("dynamic_1")
                   )
                 )
         ,

         # Municipality KPI Groups tab content
         tabItem(tabName = "municipality_kpi_groups",
                   fluidRow(
                     layout_columns(
                       fill = FALSE,
                       textInput("search_municipality_title_2","Municipality Name", value = "", width="100%",placeholder = "Municipality Name, please input Linköping")
                     ),
                     dataTableOutput("dynamic_2")
                   )
                ),
         # Municipality KPI tab content
         tabItem(tabName = "municipality_kpi",
                   fluidRow(
                     layout_columns(
                       fill = FALSE,
                
                       textInput("search_kpis_3","KPI",value="N00914",placeholder = "KPI,please input N00914",width="100%"),
                       textInput("search_municipality_title_3","Municipality ID",value="1283",placeholder = "Municipality Id, please input 1283",width="100%"),
                       textInput("search_years_3","YEAR",value="",placeholder = "Year, please input 2020",width="100%")
                     ),
                     dataTableOutput("dynamic_3")
                   )
              ),
         # Special Reports
         tabItem(tabName = "reports",
                 fluidRow(
                   layout_columns(
                     fill = FALSE,
                     selectInput("search_cityname_4", "Municipality", choices = c("Linköping", "Malmö")),
                     selectInput("search_kpis_4", "Kpis", choices = c("U40455")),
                     selectInput("search_years_4", "Start Year", selected = NULL,choices = c(2015:format(Sys.Date(), "%Y"))),
                   ),
                   plotOutput("graph_report")
                 )
            )
       )
     )
   )
)

