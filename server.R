# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)

shinyServer(function(input, output, clientData, session) {
        reactiveVals = reactiveValues(prjStart = NULL,
                                      prjEnd = NULL,
                                      prjData = NULL)
        
        observe({
                prj = input$project
                reactiveVals$prjData = rawData[project == prj,]
                start = reactiveVals$prjData[, min(date)]
                end = reactiveVals$prjData[, max(date)]
                origin = "1970-01-01"
                reactiveVals$prjStart = as.Date(as.POSIXct(start, origin = origin))
                reactiveVals$prjEnd = as.Date(as.POSIXct(end, origin = origin))
                
        })
        
        output$downloadData <- downloadHandler(
                filename = function() {
                        "maintenance-activity-data.csv"
                },
                content = function(file) {
                        data = rawData[, .(
                                CommitId = commitId,
                                Project = project,
                                Contributor = authorMail,
                                Date = as.Date(
                                        as.POSIXct(date, origin =  "1970-01-01"),
                                        '%m/%d/%y'
                                ),
                                MaintenanceType = as.factor(
                                        ifelse(
                                                predictedCat == "a",
                                                "Adaptive",
                                                ifelse(
                                                        predictedCat == "c",
                                                        "Corrective",
                                                        "Perfective"
                                                )
                                        )
                                ),
                                Comment = gsub("\\[PATCH \\d+/\\d+\\] ", "", comment)
                        )]
                        
                        write.csv(data, file, row.names = FALSE)
                }
        )
        
        output$reportPeriod = renderUI({
                if (!isTruthy(reactiveVals$prjStart) ||
                    !isTruthy(reactiveVals$prjEnd)) {
                        return(NULL)
                }
                
                dateRangeInput(
                        "reportDateRange",
                        "Period of interest",
                        start  = reactiveVals$prjStart,
                        min  = reactiveVals$prjStart,
                        end    = reactiveVals$prjEnd,
                        max    = reactiveVals$prjEnd,
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
                        min = step * 4,
                        max = max,
                        value = value,
                        post = " days",
                        step = step,
                        ticks = FALSE
                )
        })
        
        output$prjLink = renderUI({
                tags$p("Visit ",
                       tags$a(href = repoInfo[project == input$project, repo], input$project),
                       " on GitHub.")
        })
        
        output$tabs <- renderUI({
                tabsetPanel(
                        id = "menu",
                        tabPanel(
                                "Project Activity",
                                tags$div(
                                        tags$br(),
                                        tags$p(
                                                "Tip: click",
                                                tags$b("inside"),
                                                " the bar sections to view the individual commits the comprise them."
                                        )
                                ),
                                plotlyOutput("prjTabMaintBars"),
                                dataTableOutput("prjTabSelectedData")
                        ),
                        tabPanel(
                                "Contributor Activity",
                                tags$br(),
                                radioButtons(
                                        "devKey",
                                        "Identify contributors by:",
                                        choices = c("Name only", "Email only", "Both")
                                ),
                                uiOutput("contributor"),
                                tags$div(
                                        tags$br(),
                                        tags$span(
                                                "Tip: click",
                                                tags$b("inside"),
                                                " the bar sections to view the individual commits the comprise them."
                                        )
                                ),
                                tags$br(),
                                plotlyOutput("devTabMaintBars"),
                                dataTableOutput("devTabSelectedData")
                        )
                )
        })
        
        output$contributor = renderUI({
                prjName = input$project
                if (input$devKey == "Name only") {
                        selectInput(
                                "contributor",
                                "Contributor id:",
                                choices = reactiveVals$prjData[, .(name = unique(authorName))]$name,
                                width = "100%"
                        )
                } else if (input$devKey == "Email only") {
                        selectInput(
                                "contributor",
                                "Contributor name:",
                                choices = reactiveVals$prjData[, .(name = unique(authorMail))]$name,
                                width = "100%"
                        )
                } else {
                        selectInput(
                                "contributor",
                                "Contributor name:",
                                choices = reactiveVals$prjData[, .(name = unique(
                                        paste(authorName, "(", authorMail, ")")
                                ))]$name,
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
                        as.Date(as.POSIXct(min + (
                                floor((date - min) / window) * window
                        ), origin = "1970-01-01"))
                }
                agg = reactiveVals$prjData[date >= min &
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
                if (!isTruthy(input$reportWindowSize) ||
                    !isTruthy(input$contributor)) {
                        return(NULL)
                }
                
                window = input$reportWindowSize * 24 * 60 * 60
                min = as.numeric(as.POSIXct(input$reportDateRange[1]))
                max = as.numeric(as.POSIXct(input$reportDateRange[2]))
                bucketize = function(date) {
                        as.Date(as.POSIXct(min + (
                                floor((date - min) / window) * window
                        ), origin = "1970-01-01"))
                }
                
                if (input$devKey == "Name only") {
                        filteredByContributor = reactiveVals$prjData[authorName == input$contributor,]
                        agg = filteredByContributor[date >= min &
                                                            date <= max,
                                                    .(
                                                            Adaptive = sum(predictedCat == "a"),
                                                            Corrective = sum(predictedCat == "c"),
                                                            Perfective = sum(predictedCat == "p")
                                                    ),
                                                    by = list(date = bucketize(date))]
                } else if (input$devKey == "Email only") {
                        filteredByContributor = reactiveVals$prjData[authorMail == input$contributor,]
                        agg = filteredByContributor[date >= min &
                                                            date <= max,
                                                    .(
                                                            Adaptive = sum(predictedCat == "a"),
                                                            Corrective = sum(predictedCat == "c"),
                                                            Perfective = sum(predictedCat == "p")
                                                    ),
                                                    by = list(date = bucketize(date))]
                } else {
                        filteredByContributor = reactiveVals$prjData[paste(authorName, "(", authorMail, ")") == input$contributor,]
                        agg = filteredByContributor[date >= min &
                                                            date <= max,
                                                    .(
                                                            Adaptive = sum(predictedCat == "a"),
                                                            Corrective = sum(predictedCat == "c"),
                                                            Perfective = sum(predictedCat == "p")
                                                    ),
                                                    by = list(date = bucketize(date))]
                }
                
                
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
        
        observeEvent(input$exploreRawData, {
                output$rawDataTable = DT::renderDataTable({
                        rawData[, .(
                                CommitId = commitId,
                                Project = project,
                                Contributor = authorMail,
                                Date = as.Date(
                                        as.POSIXct(date, origin =  "1970-01-01"),
                                        '%m/%d/%y'
                                ),
                                MaintenanceType = as.factor(
                                        ifelse(
                                                predictedCat == "a",
                                                "Adaptive",
                                                ifelse(
                                                        predictedCat == "c",
                                                        "Corrective",
                                                        "Perfective"
                                                )
                                        )
                                ),
                                Comment = gsub("\\[PATCH \\d+/\\d+\\] ", "", comment)
                        )]
                },
                filter = "top",
                rownames = F,
                options = list(
                        searching = TRUE,
                        lengthChange = FALSE,
                        autoWidth = FALSE
                ))
        })
        
        
        observe({
                clickData <- event_data("plotly_click", source = "prj_subset")
                
                if (length(clickData) == 0) {
                        return(NULL)
                }
                
                window = isolate(input$reportWindowSize) * 24 * 60 * 60
                min = as.numeric(as.POSIXct(isolate(input$reportDateRange[1])))
                max = as.numeric(as.POSIXct(isolate(input$reportDateRange[2])))
                toBucket = function(date) {
                        as.Date(as.POSIXct(min + (
                                floor((date - min) / window) * window
                        ), origin = "1970-01-01"))
                }
                chosenDate = as.Date(clickData$x)
                bla = rawData[project == isolate(input$project),]
                agg = bla[date >= min &
                                  date <= max &
                                  toBucket(date) == chosenDate,
                          .(
                                  CommitId = commitId,
                                  Contributor = authorMail,
                                  Date = as.Date(as.POSIXct(date, origin = "1970-01-01")),
                                  Class = ifelse(
                                          predictedCat == "a",
                                          "Adaptive",
                                          ifelse(
                                                  predictedCat == "c",
                                                  "Corrective",
                                                  "Perfective"
                                          )
                                  ),
                                  Comment = gsub("\\[PATCH \\d+/\\d+\\] ",
                                                 "",
                                                 comment)
                          )]
                
                chosenClass = ifelse(
                        clickData$curveNumber == 0,
                        "Adaptive",
                        ifelse(
                                clickData$curveNumber == 1,
                                "Perfective",
                                "Corrective"
                        )
                )
                
                tblData = agg[as.character(Class) == chosenClass, ]
                
                if (nrow(tblData) == 0) {
                        return(NULL)
                }
                
                showModal(
                        modalDialog(
                                DT::renderDataTable(tblData[, .(CommitId, Comment)],
                                                    selection = 'none'),
                                title = paste0(
                                        "The ",
                                        isolate(input$project),
                                        " project ",
                                        " had the following ",
                                        chosenClass,
                                        " commits",
                                        " during the ",
                                        isolate(input$reportWindowSize),
                                        " days starting from ",
                                        format(chosenDate, "%d-%m-%Y"),
                                        " :"
                                ),
                                size = "l",
                                easyClose = TRUE
                        )
                )
        })
        
        observe({
                clickData <- event_data("plotly_click", source = "dev_subset")
                
                if (length(clickData) == 0) {
                        return(NULL)
                }
                
                window = isolate(input$reportWindowSize) * 24 * 60 * 60
                min = as.numeric(as.POSIXct(isolate(input$reportDateRange[1])))
                max = as.numeric(as.POSIXct(isolate(input$reportDateRange[2])))
                toBucket = function(date) {
                        as.Date(as.POSIXct(min + (
                                floor((date - min) / window) * window
                        ), origin = "1970-01-01"))
                }
                chosenDate = as.Date(clickData$x)
                dev = isolate(input$contributor)
                if (input$devKey == "Name only") {
                        bla = reactiveVals$prjData[authorName == dev, ]
                        
                } else if (input$devKey == "Email only") {
                        bla = reactiveVals$prjData[authorMail == dev, ]
                } else {
                        bla = reactiveVals$prjData[paste(authorName, "(", authorMail, ")") == dev, ]
                }
                
                agg = bla[date >= min &
                                  date <= max &
                                  toBucket(date) == chosenDate,
                          .(
                                  CommitId = commitId,
                                  Date = as.Date(as.POSIXct(date, origin = "1970-01-01")),
                                  Class = ifelse(
                                          predictedCat == "a",
                                          "Adaptive",
                                          ifelse(
                                                  predictedCat == "c",
                                                  "Corrective",
                                                  "Perfective"
                                          )
                                  ),
                                  Comment = gsub("\\[PATCH \\d+/\\d+\\] ",
                                                 "",
                                                 comment)
                          )]
                
                chosenClass = ifelse(
                        clickData$curveNumber == 0,
                        "Adaptive",
                        ifelse(
                                clickData$curveNumber == 1,
                                "Perfective",
                                "Corrective"
                        )
                )
                
                tblData = agg[as.character(Class) == chosenClass, ]
                
                if (nrow(tblData) == 0) {
                        return(NULL)
                }
                
                showModal(
                        modalDialog(
                                DT::renderDataTable(
                                        tblData[, .(CommitId, Comment)],
                                        selection = 'none',
                                        options = list(
                                                searching = FALSE,
                                                lengthChange = FALSE
                                        )
                                ),
                                title = paste0(
                                        isolate(input$contributor),
                                        " performed the following ",
                                        chosenClass,
                                        " commits",
                                        " during the ",
                                        isolate(input$reportWindowSize),
                                        " days starting from ",
                                        format(chosenDate, "%d-%m-%Y"),
                                        " :"
                                ),
                                size = "l",
                                easyClose = TRUE
                                
                        )
                )
        })
        
})
