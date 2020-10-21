#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)

# Contents of app header
header <- dashboardHeader(
    title = "WONDER VISION"
)

# Contents of page Sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Exploration Dashboard", tabName = "exploration_dashboard",
                 icon = icon("car-crash")
        ),
        menuItem("About", tabName = "about",
                 icon = icon("info"))
    )
)

# Contents of page dashboard Body
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "exploration_dashboard",
                fluidRow(
                    h3("Select filters for exploring the CDC WONDER database.")
                ),
                fluidRow(
                    column(6,
                           uiOutput("causeOfDeathFilter")
                           ),
                    column(2,
                           radioButtons("g", "Gender:",
                                        choiceNames = list(
                                            "All individuals", "Male", "Female"
                                        ),
                                        choiceValues = list("all", "m", "f")
                            )
                    ),
                    column(2,
                           radioButtons("r", "Race:",
                                        choiceNames = list(
                                            "All individuals", "White", "Asian/Pacific Islander", "Black/African American"
                                        ),
                                        choiceValues = list("all", "w", "api", "baa")
                           )
                    ),
                    column(2,
                           radioButtons("c", "Color plots by:",
                                        choiceNames = list(
                                            "Gender", "Race"
                                        ),
                                        choiceValues = list("g", "r")
                            )
                    )
                           
                ),
                fluidRow(
                    box(plotOutput("totalAnnual", height="200px")
                        ),
                    box(plotOutput("totalMonthly", height="200px")
                        )
                ),
                fluidRow(
                    box(plotOutput("ageDist", height="200px")
                        )#,
                    #box(plotOutput("totalRegion")
                     #   )
                ),
                fluidRow(
                    column(6,
                           uiOutput("poweredBy"))
                )
        ),
        tabItem(tabName = "about",
                uiOutput("about")
        )
    )
    
)

# Create App UI
ui <- dashboardPage(header, sidebar, body)
