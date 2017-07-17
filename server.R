# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)

shinyServer(function(input, output, clientData, session) {
        vals = reactiveValues()
        vals$prjStart = NULL
        vals$prjEnd = NULL
        vals$devId = NULL
        
        output$reportPeriod = renderUI({
                if (!isTruthy(vals$prjStart) || !isTruthy(vals$prjEnd)) {
                        return(NULL)
                }
                
                dateRangeInput(
                        "reportDateRange",
                        "Period of interest",
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
                        "Activity bucket",
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
        
        output$rawDataTableHelp = renderUI(tags$div(
                tags$br(),
                tags$span(
                        "Raw commit data for project ",
                        tags$b(input$project),
                        "dating ",
                        "from ", 
                        tags$b(input$reportDateRange[1]),
                        "to ", 
                        tags$b(input$reportDateRange[2]),
                        "."),
                tags$br(),
                tags$br()
        ))
        
        output$prj_clickHelp = renderUI(tags$div(
                tags$br(),
                tags$span(
                        "Tip: click",
                        tags$b("inside"),
                        " the bar sections to view the individual commits the comprise them."
                )
        ))
        
        output$dev_clickHelp = renderUI(tags$div(
                tags$br(),
                tags$span(
                        "Tip: click",
                        tags$b("inside"),
                        " the bar sections to view the individual commits the comprise them."
                )
        ))
        
        observe({
                output$tabs <- renderUI({
                        tabsetPanel(
                                id = "menu",
                                tabPanel(
                                        "Project-Activity",
                                        htmlOutput("prj_clickHelp"),
                                        plotlyOutput("prjTabMaintBars"),
                                        dataTableOutput("prjTabSelectedData")
                                ),
                                tabPanel(
                                        "Contributor-Activity",
                                        tags$br(),
                                        radioButtons("devKey",
                                                     "Identify contributors by:",
                                                     choices = c("Name only", "Email only", "Both")),
                                        uiOutput("contributor"),
                                        htmlOutput("dev_clickHelp"),
                                        tags$br(),
                                        plotlyOutput("devTabMaintBars"),
                                        dataTableOutput("devTabSelectedData")
                                ),
                                tabPanel(
                                        "Raw-Data",
                                        htmlOutput("rawDataTableHelp"),
                                        dataTableOutput("rawDataTable")
                                )
                        )
                })
        })
        
        output$contributor = renderUI({
                if(input$devKey == "Name only") {
                        vals$devId = Vectorize(function(name, email) {
                                name
                        })
                        selectInput("contributor",
                                    "Contributor name:",
                                    choices = rawData[, .(name = unique(authorName))][order(name)]$name,
                                    width = "100%"
                        )
                } else if(input$devKey == "Email only") {
                        vals$devId = Vectorize(function(name, email) {
                                email
                        })
                        selectInput("contributor",
                                    "Contributor name:",
                                    choices = rawData[, .(name = unique(authorMail))][order(name)]$name,
                                    width = "100%"
                        )
                } else {
                        vals$devId = Vectorize(function(name, email) {
                                paste(name, "(", email, ")")
                        })
                        selectInput("contributor",
                                    "Contributor name:",
                                    choices = rawData[, .(name = unique(paste(authorName, "(", authorMail, ")")))][order(name)]$name,
                                    width = "100%"
                        )
                }
        })
        
        output$prjTabMaintBars = renderPlotly({
                if (!isTruthy(input$reportWindowSize)) {
                        return(NULL)
                }
                
                window = input$reportWindowSize * 24 * 60 * 60
                min = as.numeric(as.POSIXct(input$reportDateRange[1]))
                max = as.numeric(as.POSIXct(input$reportDateRange[2]))
                bucketize = function(date) {
                        as.Date(
                                as.POSIXct(min + (
                                        floor((date - min) / window) * window
                                ), origin = "1970-01-01")
                        )
                }
                agg = rawData[project == input$project &
                                      date >= min &
                                      date <= max,
                              .(
                                      Adaptive = sum(predictedCat == "a"),
                                      Corrective = sum(predictedCat == "c"),
                                      Perfective = sum(predictedCat == "p")
                              ),
                              by = list(date = bucketize(date))]
                
                plot_ly(
                        agg,
                        x = ~ date,
                        y = ~ Adaptive,
                        name = 'Adaptive',
                        type = 'bar',
                        source = "prj_subset"
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
        
        output$devTabMaintBars = renderPlotly({
                if (!isTruthy(input$reportWindowSize) || !isTruthy(vals$devId)) {
                        return(NULL)
                }
                
                window = input$reportWindowSize * 24 * 60 * 60
                min = as.numeric(as.POSIXct(input$reportDateRange[1]))
                max = as.numeric(as.POSIXct(input$reportDateRange[2]))
                bucketize = function(date) {
                        as.Date(
                                as.POSIXct(min + (
                                        floor((date - min) / window) * window
                                ), origin = "1970-01-01")
                        )
                }
                
                agg = rawData[project == input$project &
                                      date >= min &
                                      date <= max &
                                      vals$devId(authorName, authorMail) == input$contributor,
                              .(
                                      Adaptive = sum(predictedCat == "a"),
                                      Corrective = sum(predictedCat == "c"),
                                      Perfective = sum(predictedCat == "p")
                              ),
                              by = list(date = bucketize(date))]
                
                plot_ly(
                        agg,
                        x = ~ date,
                        y = ~ Adaptive,
                        name = 'Adaptive',
                        type = 'bar',
                        source = "dev_subset"
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
        
        output$rawDataTable = renderDataTable({
                min = as.numeric(as.POSIXct(input$reportDateRange[1]))
                max = as.numeric(as.POSIXct(input$reportDateRange[2]))
                rawData[project == input$project & date >= min & date <= max, .(
                        CommitId = commitId,
                        Contributor = authorMail,
                        Date = as.Date(
                                as.POSIXct(date, origin =  "1970-01-01"),
                                '%m/%d/%y'
                        ),
                        Class = ifelse(predictedCat == "a", "Adaptive", ifelse(predictedCat == "c", "Corrective", "Perfective")),
                        Comment = gsub("\\[PATCH \\d+/\\d+\\] ", "", comment)
                )]
        }, options = list(
                searching = FALSE,
                lengthChange = FALSE
        ))
        
        output$prjTabSelectedData <- renderDataTable({
                s <- event_data("plotly_click", source = "prj_subset")
                
                if (length(s) == 0) {
                        return(NULL)
                }
                
                window = input$reportWindowSize * 24 * 60 * 60
                min = as.numeric(as.POSIXct(input$reportDateRange[1]))
                max = as.numeric(as.POSIXct(input$reportDateRange[2]))
                toBucket = function(date) {
                        as.Date(as.POSIXct(min + (floor((date - min) / window) * window), origin = "1970-01-01"))
                }
                chosenDate = as.Date(s$x)
                
                agg = rawData[project == input$project &
                                      date >= min &
                                      date <= max &
                                      toBucket(date) == chosenDate,
                              .(
                                      CommitId = commitId,
                                      Contributor = authorMail,
                                      Date = as.Date(as.POSIXct(date, origin = "1970-01-01")),
                                      Class = ifelse(predictedCat == "a", "Adaptive", ifelse(predictedCat == "c", "Corrective", "Perfective")),
                                      Comment = gsub("\\[PATCH \\d+/\\d+\\] ",
                                                     "",
                                                     comment)
                              )]
                
                chosenClass = ifelse(s$curveNumber == 0, "Adaptive", ifelse(s$curveNumber == 1, "Perfective", "Corrective"))
                agg[as.character(Class) == chosenClass,]
        },
        options = list(
                searching = FALSE,
                lengthChange = FALSE
        ))
        
        output$devTabSelectedData <- renderDataTable({
                s <- event_data("plotly_click", source = "dev_subset")
                
                if (length(s) == 0) {
                        return(NULL)
                }
                
                window = input$reportWindowSize * 24 * 60 * 60
                min = as.numeric(as.POSIXct(input$reportDateRange[1]))
                max = as.numeric(as.POSIXct(input$reportDateRange[2]))
                toBucket = function(date) {
                        as.Date(as.POSIXct(min + (floor((date - min) / window) * window), origin = "1970-01-01"))
                }
                chosenDate = as.Date(s$x)
                
                agg = rawData[project == input$project &
                                      date >= min &
                                      date <= max &
                                      toBucket(date) == chosenDate &
                                      vals$devId(authorName, authorMail) == input$contributor,
                              .(
                                      CommitId = commitId,
                                      Date = as.Date(as.POSIXct(date, origin = "1970-01-01")),
                                      Class = ifelse(predictedCat == "a", "Adaptive", ifelse(predictedCat == "c", "Corrective", "Perfective")),
                                      Comment = gsub("\\[PATCH \\d+/\\d+\\] ",
                                                     "",
                                                     comment)
                              )]
                
                chosenClass = ifelse(s$curveNumber == 0, "Adaptive", ifelse(s$curveNumber == 1, "Perfective", "Corrective"))
                agg[as.character(Class) == chosenClass,]
        },
        options = list(
                searching = FALSE,
                lengthChange = FALSE
        ))
        
        output$myText1 = renderText({
                "bla bla bla1 "
        })
})
