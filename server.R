# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(ggplot2)

shinyServer(function(input, output, clientData, session) {
        vals = reactiveValues()
        vals$prjStart = NULL
        vals$prjEnd = NULL
        
        output$event <- renderPrint({
                d <- event_data("plotly_hover")
                if (is.null(d))
                        "Hover on a point!"
                else
                        d
        })
        
        output$reportPeriod = renderUI({
                if (!isTruthy(vals$prjStart) || !isTruthy(vals$prjEnd)) {
                        return(NULL)
                }
                
                dateRangeInput(
                        "reportDateRange",
                        "Period",
                        start  = vals$prjStart,
                        min  = vals$prjStart,
                        end    = vals$prjEnd,
                        max    = vals$prjEnd,
                        format = "dd/mm/yy",
                        separator = " - "
                )
        })
        
        output$reportWindow = renderUI({
                if (!isTruthy(input$reportDateRange) ||
                    !isTruthy(input$reportDateRange[1]) ||
                    !isTruthy(input$reportDateRange[2])) {
                        return(NULL)
                }
                
                start = input$reportDateRange[1]
                end = input$reportDateRange[2]
                step = 7
                diff = difftime(end, start, units = "days")
                max_floor = floor(as.numeric(diff) / step) * step
                max = max_floor + step
                prevValue = isolate(input$reportWindowSize)
                value = if (isTruthy(prevValue) &&
                            prevValue <= max) {
                        prevValue
                } else {
                        max(step, step * 4)
                }
                
                sliderInput(
                        "reportWindowSize",
                        "Activity Window",
                        min = step,
                        max = max,
                        value = value,
                        post = " days",
                        step = step,
                        ticks = FALSE
                )
        })
        
        observe({
                project = input$project
                start = rawData[project == project, min(date)]
                end = rawData[project == project, max(date)]
                origin = "1970-01-01"
                vals$prjStart = as.Date(as.POSIXct(start, origin = origin))
                vals$prjEnd = as.Date(as.POSIXct(end, origin = origin))
        })
        
        
        output$tabs = renderText("Please select a project ...")
        
        observe({
                output$tabs <- renderUI({
                        tabsetPanel(
                                id = "menu",
                                tabPanel(
                                        "Project-Overview",
                                        plotlyOutput("maintBars")
                                ),
                                tabPanel(
                                        "Developer-Info",
                                        tableOutput("myText1")
                                ),
                                tabPanel(
                                        "Raw-Data",
                                        dataTableOutput("rawDataTable")
                                )
                        )
                })
                
                
                output$maintBars = renderPlotly({
                        if (!isTruthy(input$reportWindowSize)) {
                                return(NULL)
                        }
                        
                        window = input$reportWindowSize * 24 * 60 * 60
                        min = as.numeric(as.POSIXct(input$reportDateRange[1]))
                        max = as.numeric(as.POSIXct(input$reportDateRange[2]))
                        agg = rawData[project == input$project &
                                              date >= min &
                                              date <= max,
                                      .(
                                              Adaptive = sum(predictedCat == "a"),
                                              Corrective = sum(predictedCat == "c"),
                                              Perfective = sum(predictedCat == "p")
                                      ),
                                      by = list(date = as.Date(
                                              as.POSIXct(min + (
                                                      floor((date - min) / window) * window
                                              ), origin = "1970-01-01")
                                      ))]
                        
                        plot_ly(
                                agg,
                                x = ~ date,
                                y = ~ Adaptive,
                                name = 'Adaptive',
                                type = 'bar'
                        ) %>%
                                add_trace(y = ~ Perfective,
                                          name = 'Perfective') %>%
                                add_trace(y = ~ Corrective,
                                          name = 'Corrective') %>%
                                layout(
                                        yaxis = list(title = 'Commits'),
                                        barmode = 'stack',
                                        xaxis = list(
                                                tickvals = agg$date,
                                                tickangle = 45,
                                                title = "",
                                                tickformat = "%d-%m-%Y"
                                        ),
                                        margin = list(b = 150)
                                )  %>%
                                config(displayModeBar = F)
                })
                output$myText1 = renderText({
                        "bla bla bla1 "
                })
                output$rawDataTable = renderDataTable({
                        x  = 1
                        rawData[project == input$project, .(
                                CommitId = commitId,
                                Date = as.Date(as.POSIXct(date, origin =  "1970-01-01"), '%m/%d/%y'),
                                Class = predictedCat,
                                Comment = gsub("\\[PATCH \\d+/\\d+\\] ", "", comment)
                        )]
                }, options = list(searching = FALSE, lengthChange = FALSE))
        })
})
