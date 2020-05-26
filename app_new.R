options(stringsAsFactors=FALSE)

library(lubridate)
library(tidyverse)
library(shiny)
vgsales <- read_csv('/Users/Justin/Documents/SCHOOL/DataScience/proj/vgsales.csv')


ui <- navbarPage("Video Games",
                 tabPanel("Video Games Over the Years",
                          helpText('Video Game Sales Have been increasing ever since their invetion. This data set stops recording relevant data after 2016'),
                          helpText('Wii Sports is an extreme outlier as it was included with every single console for free'),
                          mainPanel(
                              plotOutput(outputId="wholePlot")
                          )
                 ),
                 tabPanel("How Well Did It Sell?",
                          helpText('Pick / Type any game that you can think of and see how well it sold on each console on differnt regions'),
                          sidebarPanel(
                              selectizeInput('games',label = h3('Choose Game'),choices = NULL)
                          ),
                          mainPanel(
                              plotOutput(outputId="salesPlot")
                          )
                          ),
                 tabPanel("What Year was Top Tier",
                          helpText('Pick a range of years to see what games were the best selling of the time period'),
                          sidebarPanel(
                              sliderInput("slider2", label = h3("Choose Year Range"), min = 1980, max = 2020, value = c(1980, 2020)),
                          ),
                          mainPanel(
                             plotOutput(outputId="yearPlot")
                          )
                          )
)


server <- function(input, output, session) {
    
    updateSelectizeInput(session, 'games', choices = vgsales$Name, server = TRUE)
    
    output$range <- renderPrint({ input$slider2 })
    
    output$salesPlot <- renderPlot({
        
        game_info <- vgsales%>%
            filter(Name == input$games)%>%
            gather(Location, Sales, NA_Sales:Global_Sales)
            
        
        
        ggplot(game_info, aes(x = reorder(Location, -Sales), y = Sales, fill= Location)) +
            scale_x_discrete(labels=c("NA_Sales" = "NA","EU_Sales" = "EU","JP_Sales" = "JP","Other_Sales" = "Other","Global_Sales" = "Global"))+
            facet_wrap(~ Platform)+
            geom_bar(stat='identity') +
            geom_text(aes(label = Sales), size = 3, hjust = 0.5, vjust = 2, position ="stack")+
            xlab('Region')+
            ylab('Number of Copies Sold (millions)')+
            theme_bw()
    })
    
    
    output$yearPlot <- renderPlot({
        year_test <- vgsales %>%
            filter(as.numeric(Year) >= input$slider2[1]) %>%
            filter(as.numeric(Year) <= input$slider2[2]) %>%
            mutate(Name = paste(Name,' (',Platform,')', sep = '')) %>%
            mutate(Total_Sales = NA_Sales+EU_Sales+JP_Sales+Other_Sales+Global_Sales) %>%
            select(Name, Year,Total_Sales)
        year_test = head(year_test, n = 10)
        year_test = year_test[order(year_test$Total_Sales, decreasing = TRUE),]
        
        ggplot(year_test, aes(x= reorder(Name,Total_Sales),y=Total_Sales, fill = Name))+
            geom_bar(stat='identity')+
            geom_text(aes(label = Year), size = 3, hjust = 1.2, vjust = 0.2, position ="stack")+
            coord_flip() +
            theme(legend.position = "none") +
            xlab('Name')+
            ylab('Number of Copies Sold (millions)') +
            theme_bw()
    })
    
    output$wholePlot <- renderPlot({
        year_sales <- vgsales %>%
            mutate(Total_Sales = NA_Sales+EU_Sales+JP_Sales+Other_Sales+Global_Sales) %>%
            mutate(Year_fix = year(as.Date(Year, '%Y'))) %>%
            filter(is.numeric(Total_Sales)) %>%
            select(Year_fix, Total_Sales)
        
        year_sales <- aggregate(year_sales, by = list(year_sales$Year_fix), FUN = sum)
        
        ggplot(year_sales, aes(x = Group.1, y = Total_Sales))+
            geom_line() + 
            theme(axis.text.x = element_text(angle = 45))
    })
    # output$flightPlot <- renderPlot({
    #     req(input$angle)  # Ensure this number is available to avoid error
    # 
    #     flightsPerDay <- flights %>%
    #         filter(time_hour >= input$dateRange[1] &
    #                    time_hour <= input$dateRange[2] &
    #                    origin %in% input$origin) %>%
    #         mutate(date=as_date(time_hour)) %>%
    #         count(origin, date)
    # 
    #     ggplot(flightsPerDay, aes(x=date, y=n, col=origin)) +
    #         labs(x="", y="# of flights") +
    #         geom_line() +
    #         scale_x_date(date_labels="%a %b %e", date_breaks="1 day") +
    #         theme_bw() +
    #         theme(axis.text.x=element_text(angle=input$angle, hjust=1))
    # })
}

# Run the application 
shinyApp(ui=ui, server=server)
