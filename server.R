# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

shinyServer(function(input, output, clientData, session) {
        
        vals = reactiveValues()
        vals$prjStart = NULL
        vals$prjEnd = NULL
        
        
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
                value = if(isTruthy(prevValue) && prevValue <= max) {
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
                                        plotOutput("maintBars")
                                ),
                                tabPanel(
                                        "Developer-Info",
                                        tableOutput("myText1")
                                ),
                                tabPanel("Raw-Data",
                                         tableOutput("myText2"))
                        )
                })
                
                
                output$maintBars = renderPlot({
                        if(!isTruthy(input$reportWindowSize)) {
                                return(NULL)
                        }
                        
                        window = input$reportWindowSize * 24 * 60 * 60
                        min = as.numeric(as.POSIXct(input$reportDateRange[1]))
                        max = as.numeric(as.POSIXct(input$reportDateRange[2]))
                        melted = melt(rawData[project == input$project & date >= min & date <= max,
                                              .(
                                                      Adaptive = sum(predictedCat == "a"),
                                                      Corrective = sum(predictedCat == "c"),
                                                      Perfective = sum(predictedCat == "p")
                                              ),
                                              by = list(date = min + (floor((date - min) / window) * window))],
                                      id = c("date"))
                        
                        ggplot(data = melted, aes(x = as.Date(as.POSIXct(date, origin = "1970-01-01")), y = value, fill = variable)) + 
                                geom_bar(stat = "identity") +
                                ylab("Commits") + 
                                ggtitle(paste0("\n", "Project: ", input$project, "\n")) +
                                theme(
                                        legend.position = "top",
                                        legend.title = element_blank(),
                                        axis.text = element_text(size = 12),
                                        axis.title.x = element_blank(),
                                        axis.title.y = element_text(size = 18),
                                        legend.text = element_text(size = 15),
                                        axis.text.x = element_text(angle = 90, hjust = 1),
                                        plot.title = element_text(size=22, hjust = 0.5)
                                ) + 
                                scale_fill_brewer(palette="Set1") + 
                                scale_x_date(breaks = as.Date(as.POSIXct(melted$date, origin =  "1970-01-01")))
                })
                output$myText1 = renderText({
                        "bla bla bla1 "
                })
                output$myText2 = renderText({
                        "bla bla bla2 "
                })
        })
})
