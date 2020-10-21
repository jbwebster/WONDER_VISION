
# Ideas of things to include
# Has the median age of death changed over time?
# Incorporate more plots that include race statistics


library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(maps)

# Define server logic 
shinyServer(function(input, output) {
    # Load entire dataset. User input will
    # dynamically create subsets of the data for plotting
    #data <- read.csv("/Data/CDC_Wonder_Data.csv", header=T)
    #setwd("/Users/jacebwebster/Desktop/RShiny/CDC/US_WONDER_VIS/")
    data <- read.csv("Data/CDC_Wonder_Top15.csv", header=T)
    data$Cause.of.death <- as.character(data$Cause.of.death)

    
    subsetDF <- function(input,data) ({
         cod <- input$causeOfDeathFilter
         if (cod != "All Reported Causes") {
             df_cod <- data[data$Cause.of.death == cod,]           
         } else {
             df_cod <- data
         }
        if (input$g == "m") {
            df_cod <- df_cod[df_cod$Gender == "Male",]
        } else if (input$g == "f") {
            df_cod <- df_cod[df_cod$Gender == "Female",]
        }
        if (input$r == "w") {
            df_cod <- df_cod[df_cod$Race == "White",]
        } else if (input$r == "api") {
            df_cod <- df_cod[df_cod$Race == "Asian or Pacific Islander",]
        } else if (input$r == "baa") {
            df_cod <- df_cod[df_cod$Race == "Black or African American",]
        } 
        return(df_cod)
    })
        
    
    
    output$totalAnnual <- renderPlot({
        df_cod <- subsetDF(input, data)
        df_cod <- df_cod[,c("Year", "Deaths", "Month")]
        df_cod$Date <- paste0(df_cod$Month, "-01-", df_cod$Year)
        grouped <- df_cod %>%
            group_by(Date) %>%
            summarise(totalDeath = sum(Deaths))
        grouped$Date <- as.Date(grouped$Date, format="%b-%d-%Y")
        main_title <- ""
        main_title <- "Total Deaths Over Time"
        ggplot(grouped, aes(x=Date, y=totalDeath, group=1)) +
            geom_line() +
            geom_smooth(method = 'loess', formula= y~x) +
            scale_y_continuous(labels=comma, limits = c(0,NA)) +
            scale_x_date(breaks="1 year", labels = date_format("%b, %Y")) +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(x ="Month", y = "Total Deaths",
                 title = main_title)
    })
    
    output$totalMonthly <- renderPlot({
        df_cod <- subsetDF(input, data)
        if (input$c == "g") {
            df_cod <- df_cod[,c("Month", "Deaths", "Gender")]
            grouped <- df_cod %>%
                group_by(Month, Gender) %>%
                summarise(totalDeath = sum(Deaths))
            grouped$Month <- factor(grouped$Month, levels = month.abb)
            main_title <- "Deaths by Month 1999-2018"
            ggplot(grouped, aes(x=Month, y=totalDeath, group=1, fill=Gender)) +
                geom_col() +
                scale_y_continuous(labels=comma, limits = c(0,NA)) +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(x ="Month", y = "Total Deaths",
                    title = main_title)
        } else {
            df_cod <- df_cod[,c("Month", "Deaths", "Race")]
            grouped <- df_cod %>%
                group_by(Month, Race) %>%
                summarise(totalDeath = sum(Deaths))
            grouped$Month <- factor(grouped$Month, levels = month.abb)
            main_title <- "Deaths by Month 1999-2018"
            ggplot(grouped, aes(x=Month, y=totalDeath, group=1, fill=Race)) +
                geom_col() +
                scale_y_continuous(labels=comma, limits = c(0,NA)) +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(x ="Month", y = "Total Deaths",
                     title = main_title)
        }
    
    })
    
    output$ageDist <- renderPlot({
        df_cod <- subsetDF(input, data)
        if (input$c == "g") {
            df_cod <- df_cod[,c("Age.Group", "Deaths", "Gender")]
            grouped <- df_cod %>%
                group_by(Age.Group ,Gender) %>%
                summarise(totalDeath = sum(Deaths))
            grouped$Age.Group <- factor(grouped$Age.Group,
                                        levels = c("1","1-4", "5-14", "15-24",
                                                   "25-34", "35-44", "45-54", "55-64",
                                                   "65-74", "75-84", "85+"))
            ggplot(grouped, aes(x=Age.Group, y=totalDeath, fill=Gender)) +
                geom_col() +
                scale_y_continuous(labels=comma, limits = c(0,NA)) +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(title = "Age of Deceased Persons, 1999-2018",
                    x="Age Group", y="Total Deaths")
        } else {
            df_cod <- df_cod[,c("Age.Group", "Deaths", "Race")]
            grouped <- df_cod %>%
                group_by(Age.Group ,Race) %>%
                summarise(totalDeath = sum(Deaths))
            grouped$Age.Group <- factor(grouped$Age.Group,
                                        levels = c("1","1-4", "5-14", "15-24",
                                                   "25-34", "35-44", "45-54", "55-64",
                                                   "65-74", "75-84", "85+"))
            ggplot(grouped, aes(x=Age.Group, y=totalDeath, fill=Race)) +
                geom_col() +
                scale_y_continuous(labels=comma, limits = c(0,NA)) +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(title = "Age of Deceased Persons, 1999-2018",
                     x="Age Group", y="Total Deaths")           
        }
    })
    
    # Consider removing or modifying
    # output$totalRegion <- renderPlot({
    #     df_cod <- subsetDF(input, data)
    #     df_cod <- df_cod[,c("State", "Deaths")]
    #     grouped <- df_cod %>%
    #         group_by(State) %>%
    #         summarise(totalDeaths = sum(Deaths))
    #     grouped$State <- tolower(grouped$State)
    #     colnames(grouped) <- c("region", "totalDeaths")
    #     us_map <- map_data("state")
    #     to_plot <- inner_join(us_map, grouped, by = "region")
    #     ggplot() +
    #         geom_polygon(data=to_plot, 
    #                      aes(x=long, y=lat, group=group,  fill=totalDeaths)) +
    #         labs(fill="Total Deaths",
    #              title="Death by State from 1999-2018",
    #              caption="States are not shown when they have 0 deaths.\nCounts are not controlled for population.") +
    #         scale_fill_gradient(low="#56B1F7", high="#132B43")
    # })
    
    getCausesofDeath <- reactive ({
        CoD <- as.list(unique(data$Cause.of.death))
        CoD <- c("All Reported Causes", CoD)
    })
    
    output$causeOfDeathFilter <- renderUI({
        selectizeInput(inputId = "causeOfDeathFilter",
                       label = "Cause of Death:",
                       choices = getCausesofDeath(),
                       selected = getCausesofDeath()[1],
                       multiple = FALSE,
                       options = list(create = FALSE))
    })
    
    
    output$about <- renderText({
        txt <- '<br><h3>About WONDER VISION</h3><br><br><br>
        WONDER VISION is an app built with R Shiny to visualize and explore a subset of the CDC\'s WONDER database.
        Death statistics from the top 15 most common categories for cause of death 
        from 1999-2018 in the US can be explored within the app, and can be grouped in 
        various combinations of multiple demographics. Users can apply filters within 
        the app to easily answer questions like "Which gender has the highest death rate due to unintentional accidents?", 
        "Are annual trends in deaths caused by Alzheimers consistent across different races?", and 
        "Which of the most common causes of death is most prevalent among youth?" While observations made 
        here may be interesting, it is important to remember that these are raw death count numbers and are not  
        controlled for by population size.
        <br><br>For more information regarding CDC WONDER,
        their <a href="https://wonder.cdc.gov/">website can be found here<a/>.'
    })
    
    output$poweredBy <- renderText({
        txt <- "<br><br>Powered by <b>R Shiny</b> and the <b>CDC WONDER</b> database.<br><br>"
    })

})
