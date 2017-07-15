# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(data.table)

shinyUI(navbarPage(
        "Software Maintenance Analytics",
        tabPanel("Show me!",
                 fluidPage(
                         # Application title
                         titlePanel(""),
                         # Sidebar with a slider input for number of bins
                         sidebarLayout(
                                 sidebarPanel(
                                         selectInput("project",
                                                     "Project name",
                                                     choices = rawData[, .(project = unique(project))]$project),
                                         uiOutput("reportPeriod"),
                                         uiOutput("reportWindow")
                                         # sliderInput(
                                         #         "reportWindow",
                                         #         "Activity Window",
                                         #         min = 1,
                                         #         max = 50,
                                         #         value = 30,
                                         #         post = " days"
                                         # ),
                                 ),
                                 
                                 # Show a plot of the generated distribution
                                 mainPanel(uiOutput("tabs"))
                         )
                 )),
        tabPanel("About",
                 basicPage(
                         tags$div(
                                 checked = NA,
                                 tags$h1("Software Maintenance Activities"),
                                 tags$br(),
                                 tags$p(
                                         "The research community has identified 3 main code maintenance activities:"
                                 ),
                                 tags$ul(
                                         tags$li(tags$b("Corrective:"), 
                                                 "fault fixing."),
                                         tags$li(tags$b("Perfective:"), 
                                                 "system improvements."),
                                         tags$li(tags$b("Adaptive:"), 
                                                 "new feature introduction.")
                                 ),
                                 tags$p("This demo presents the results of applying our commit classification algorithm on open source projects."),
                                 tags$p("The underlying classification algorithm is based on the following papers:"),
                                 tags$ol(
                                         tags$li(
                                                 tags$a(href = "https://www.linkedin.com/in/staslevin/", "Stanislav Levin"),
                                                 ", and ",
                                                 tags$a(href = "http://www.cs.tau.ac.il/~amiramy/", "Amiram Yehudai"),
                                                 ". \"Using Temporal and Semantic Developer-Level Information to Predict Maintenance Activity Profiles.\" Software Maintenance and Evolution (ICSME), 2016 IEEE International Conference on. IEEE, 2016."
                                         ),
                                         tags$li(
                                                 tags$a(href = "https://www.linkedin.com/in/staslevin/", "Stanislav Levin"),
                                                 ", and ",
                                                 tags$a(href = "http://www.cs.tau.ac.il/~amiramy/", "Amiram Yehudai"),
                                                 ". \"Boosting Automatic Commit Classification Into Maintenance Activities By Utilizing Source Code Changes.\"",
                                                 tags$em("Submitted for publication, 2017.")
                                         )
                                 ),
                                 tags$p(
                                         tags$span("For more information about our research visit the project's"),
                                         tags$a(href = "https://www.researchgate.net/project/Software-Evolution-Maintenance-via-Fine-Grained-Source-Code-Changes", "ResearchGate page.")
                                 ),
                                 tags$p("Feel free to contact us for any question, suggestion, or feedback.")
                         )
                 ))
))
