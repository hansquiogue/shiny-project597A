library(shiny)
library(tidyverse)
happiness_2015 <- read.csv("2015.csv")
happiness_2016 <- read.csv("2016.csv")

ave_2015 = happiness_2015 %>% 
    group_by(Region) %>%
    summarize(ave_happiness_level = mean(Happiness.Score))
ave_2016 = happiness_2016 %>% 
    group_by(Region) %>%
    summarize(ave_happiness_level = mean(Happiness.Score))

ui <- fluidPage(

    h3(titlePanel("World Happiness Level Data")),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("select_def", h3("Data Selected:"), 
                        choices = list("Countries" = 1, 
                                       "Regions" = 2),
                        selected = 1),
            radioButtons("select_year", ("Year Selected:"), 
                        choices = list("2015" = 1, 
                                       "2016" = 2),
                        selected = 1),
            sliderInput("slider", ("Happiness Score Range:"),
                        min = 1, 
                        max = 10, 
                        value = c(7,8)),
            numericInput("n_min", 
                        ("Change y-axis min value:"), 
                        value = 2.8),
            numericInput("n_max", 
                        ("Change y-axis max value:"), 
                        value = 8)),
        mainPanel(
            plotOutput("countries", brush = "plot_brush"), 
            tableOutput("data_table"),
        )
    )
)

server <- function(input, output) {
    
    output$countries <- renderPlot({
        if(input$select_def == 1){
            if(input$select_year == 1){
                happiness_2015 %>% 
                    ggplot() +
                    aes(Country, Happiness.Score, group = Region, color = Region) +
                    geom_point() + ggtitle("Happiness Levels for Countries 2015") +
                    ylim(input$n_min, input$n_max) +
                    theme(axis.text.x = element_blank(),
                        axis.ticks.x = element_blank())
            }
            else{
                happiness_2016 %>% 
                    ggplot() +
                    aes(Country, Happiness.Score, group = Region, color = Region) +
                    geom_point() + ggtitle("Happiness Levels for Countries 2016") +
                    ylim(input$n_min, input$n_max) +
                    theme(axis.text.x = element_blank(),
                        axis.ticks.x = element_blank())
            }
        } 
        else if(input$select_def == 2){
            if(input$select_year == 1){
                ave_2015 %>% 
                    ggplot() + aes(Region, ave_happiness_level, fill = Region) +
                    geom_bar(stat = "identity") +
                    scale_x_discrete(labels = c("OCE","C/E EU", "E. Asia", "LATAM", "Mid. E/N Afr.", 
                                                "NA", "SE Asia", "S. Asia", "Sub. Africa", "W. EU")) +
                    ylab("Average Happiness Levels") + 
                    ggtitle("Average Happiness Levels for Regions Around the World in 2015")
            }
            else{
                ave_2016 %>% 
                    ggplot() + aes(Region, ave_happiness_level, fill = Region) +
                    geom_bar(stat = "identity") +
                    scale_x_discrete(labels = c("OCE","C/E EU", "E. Asia", "LATAM", "Mid. E/N Afr.", 
                                                "NA", "SE Asia", "S. Asia", "Sub. Africa", "W. EU")) +
                    ylab("Average Happiness Levels") + 
                    ggtitle("Average Happiness Levels for Regions Around the World in 2016")
            }
        }
    })
    
    output$data_table <- renderTable({
        if(input$select_def == 1){
            if(input$select_year == 1){
                count = nrow(brushedPoints(happiness_2015, brush = input$plot_brush)) 
                slide_dat = happiness_2015 %>% 
                    filter(input$slider[1] <= Happiness.Score & input$slider[2] >= Happiness.Score)
                if(count == 0){  
                    slide_dat[1:4]
                }
                else{
                    brushed = brushedPoints(happiness_2015, brush = input$plot_brush)
                    brushed[1:4]
                }
            }
            else{
                count = nrow(brushedPoints(happiness_2016, brush = input$plot_brush)) 
                slide_dat = happiness_2016 %>% 
                    filter(input$slider[1] <= Happiness.Score & input$slider[2] >= Happiness.Score)
                if(count == 0){  
                    slide_dat[1:4]
                }
                else{
                    brushed = brushedPoints(happiness_2016, brush = input$plot_brush)
                    brushed[1:4]
                }
            }
        }
        else if(input$select_def == 2){
            if(input$select_year == 1){
                ave_2015 %>% arrange(desc(ave_happiness_level))
            }
            else{
                ave_2016 %>% arrange(desc(ave_happiness_level))
            }
        }
    })
}

shinyApp(ui = ui, server = server, options = list(display.mode = 'showcase'))
