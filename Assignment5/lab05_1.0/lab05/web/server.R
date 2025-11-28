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
library(shinydashboard)
library(ggplot2)

#' Define server logic 
shinyServer(
  function(input, output) {
    # 1st tab for kolada_municipality_kpi_groups api test
    observeEvent(input$search_municipality_title_1, {
      # if there have space inside the search string, ignore it, since we can not let api throw the error
      if (grepl(" ", input$search_municipality_title_1)){
        output$dynamic_1 <- renderDataTable(NULL, options = list(pageLength = 5))
      }else{
        data <- kolada_municipality_kpi_groups(search_title=input$search_municipality_title_1)
        # if  data is available , show it else show nothing
        if (data$count > 0){
          ids <- data$values$id
          titles <- data$values$title
          # convert data frame to string
          member_ids <- character()
          for(i in 1:data$count){
            member_id <- paste(data$values[2][1][[i,1]]$member_id,collapse = ',')
            
            # if too long, limit length to 100 
            if (nchar(member_id) > 100){
              member_id <- paste(substr(member_id,1,100),"...",sep="")
            }
            member_ids <- append(member_ids, member_id)
          }
          
          # make data frame and render table
          df <- data.frame(ids,member_ids,titles)
          output$dynamic_1 <- renderDataTable(df,
                                              colnames = c('ID', 'KPI ID', 'KPI Category Name'), 
                                              options = list(pageLength = 5))
        }else{
          output$dynamic_1 <- renderDataTable(NULL, options = list(pageLength = 5))
        }
      }
      
      
    })
    
    # 2nd tab for kolada_municipality api test
    observeEvent(input$search_municipality_title_2, {
      # if there have space inside the search string, ignore it, since we can not let api throw the error
      if (grepl(" ", input$search_municipality_title_2)){
        output$dynamic_2 <- renderDataTable(NULL, options = list(pageLength = 5))
      }else{
        # call api
        data <- kolada_municipality(search_title=input$search_municipality_title_2)
        if (data$count > 0){
          output$dynamic_2 <- renderDataTable(data$values, 
                                              colnames = c('Municipality ID', 'Municipality Name', 'Municipality Type'), 
                                              options = list(pageLength = 5))
        }else{
          output$dynamic_2 <- renderDataTable(NULL, options = list(pageLength = 5))
        }
      }
      
    })
    
    # 3rd tab for kolada_municipality_kpi api test
    observeEvent(ignoreInit = TRUE, 
                 list(
                   input$search_kpis_3,
                   input$search_municipality_title_3,
                   input$search_years_3), 
                 {
                    # input check, since we can not let api throw the error
                    if(is_valid_year(input$search_years_3) && 
                       nchar(input$search_municipality_title_3) > 0 &&
                       nchar(input$search_kpis_3) > 0 &&
                       !grepl(" ", input$search_years_3) &&
                       !grepl(" ", input$search_municipality_title_3) &&
                       !grepl(" ", input$search_kpis_3)){
                      
                      # call api
                      data <- kolada_municipality_kpi(kpi=input$search_kpis_3,
                                                      municipality=input$search_municipality_title_3,
                                                      year=input$search_years_3)
                      if (data$count > 0){
                        kpi <- data$values$kpi
                        municipality <- data$values$municipality
                        period <- data$values$period
                        values <- data$values$values
                        
                        # convert dataframe to string
                        vals_str <- character()
                        for(i in 1:length(values)){
                          
                          val_str <- paste(capture.output(print(values[i])),collapse = ',')
                          
                          if (nchar(val_str) > 100){
                            val_str <- paste(substr(val_str,1,100),"...",sep="")
                          }
                          vals_str <- append(vals_str, val_str)
                          
                        }
                        
                        # make a new data frame to render table
                        df <- data.frame(kpi,municipality,period,vals_str)
                        
                        output$dynamic_3 <- renderDataTable(df, 
                                                            colnames = c('KPI ID', 'Municipality ID', 'YEAR',"VALUES"), 
                                                            options = list(pageLength = 5))
                      }else{
                        output$dynamic_3 <- renderDataTable(NULL, options = list(pageLength = 5))
                      }
                    }else{
                      output$dynamic_3 <- renderDataTable(NULL, options = list(pageLength = 5))
                    }
                  })
    
    observeEvent(ignoreInit = TRUE, 
                 list(
                   input$search_cityname_4,
                   input$search_kpis_4,
                   input$search_years_4), 
                 {
                   # get municipality id from municipality name
                    municipality_id <- kolada_municipality(input$search_cityname_4)$values$id[1]
                    
                    # get the data span, like "1999,2000,2001,2003"
                    data_span <- toString(c(as.numeric(input$search_years_4):as.integer(format(Sys.Date(), "%Y"))))
                    data_span <- gsub("\\s", "", data_span)
                    
                    # api call
                    data <- kolada_municipality_kpi(
                                                    kpi = input$search_kpis_4, 
                                                    municipality = municipality_id,
                                                    year = data_span)$values
                    year_data <- data$period
                    kpi_data <- numeric()
                    for(i in 1:length(data$values)){
                      kpi_data <- append(kpi_data, data$values[[i]]$value[1])
                    }
                    
                    # make data frame
                    df <- data.frame(year_data,kpi_data)
                    
                    # liu theme
                    linkoping_theme <- theme(
                      panel.background = element_rect(fill = "white",
                                                      colour = "lightblue",
                                                      linewidth = 0.5, linetype = "solid"),
                      panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                                      colour = "white"), 
                      panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                                      colour = "white"),
                      plot.title = element_text(color="blue2", face="bold", size="14",hjust = 0.5)
                    )
                    
                    # render graph
                    output$graph_report <- renderPlot({
                      
                      ggplot(df, aes(year_data,kpi_data)) +
                        geom_point() +
                        geom_line() +
                        linkoping_theme + 
                        labs(
                          title = "YEAR ~ KPI",
                          x = "YEAR",
                          y = "KPI"
                        )
                    })
                  })
  }
)