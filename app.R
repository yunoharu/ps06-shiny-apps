library(shiny)
library(tidyverse)

uah <- read_delim("UAH-lower-troposphere-long.csv.bz2")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Overview",
             titlePanel("UAH lower troposphere Data"),
             mainPanel(
               p("UAH lower trophsphere Data is downloaded from ",
                 em("UAH."),
                 "It has ",
                 strong(ncol(uah)),
                 "columns and ",
                 strong(nrow(uah)),
                 "rows."),
               p("Here is a small (random) sample of data:"),
               tableOutput("table1")
             )
             ),
    tabPanel("Plot",
             titlePanel("Lower Troposphere"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("plot", label="Choose Plot Type",
                              choices=c("Bar", "Line"),
                              selected="Bar"),
                 radioButtons("month", label="Choose Month",
                             choices=c(1:12),
                             selected=1),
                 checkboxGroupInput("region", label="Choose Region",
                                    choices=c("globe","globe_land","globe_ocean","nh","nh_land","nh_ocean",   
                                              "sh","sh_land","sh_ocean","trpcs","trpcs_land","trpcs_ocean",
                                              "noext","noext_land","noext_ocean","soext","soext_land","soext_ocean",
                                              "nopol","nopol_land","nopol_ocean","sopol","sopol_land","sopol_ocean",
                                              "usa48","usa49","aust"),
                                    selected=c("globe","globe_land","globe_ocean","nh","nh_land","nh_ocean",   
                                               "sh","sh_land","sh_ocean","trpcs","trpcs_land","trpcs_ocean",
                                               "noext","noext_land","noext_ocean","soext","soext_land","soext_ocean",
                                               "nopol","nopol_land","nopol_ocean","sopol","sopol_land","sopol_ocean",
                                               "usa48","usa49","aust"))),
             mainPanel(
               textOutput("text1"),
               plotOutput("plot")
               ))),
    tabPanel("Table",
             titlePanel("Lower Troposphere"),
                 sidebarLayout(
                   sidebarPanel(
                     checkboxGroupInput("columns", label="Select Columns to Display:",
                                        choices = colnames(uah),
                                        selected = colnames(uah)),
                     sliderInput("year", label="Select Year Range",
                                 min=min(uah$year),
                                 max=max(uah$year),
                                 value=c(1978,2023))),
                   mainPanel(textOutput("text2"),
                             tableOutput("table2")
                )))))


server <- function(input, output){
  output$text1 <- renderText({
    uah %>% 
      filter(month %in% input$month) %>%
      filter(region %in% input$region) %>%
      nrow() %>% 
      paste("Selected Subset conains", ., "Obserbations")
  })
  output$text2 <- renderText({
    uah %>% 
      filter(year >= input$year[1],
             year <= input$year[2]) %>%
      nrow() %>% 
      paste("Selected Subset contains", ., "Obserbations")
  })
  output$plot <- renderPlot({
    if(input$plot=="Bar"){
    uah %>% 
      filter(month %in% input$month) %>%
      filter(region %in% input$region) %>% 
      group_by(year) %>% 
      summarize(avg=mean(temp, na.rm=TRUE)) %>% 
      ggplot(aes(x=year, y=avg, group=year))+
      geom_col(fill="pink", col="black")+
      labs(x="Year", y="Average Temperature")
    }else{
      uah %>% 
        filter(month %in% input$month) %>%
        filter(region %in% input$region) %>% 
        group_by(year) %>% 
        summarize(avg=mean(temp, na.rm=TRUE)) %>% 
        ggplot(aes(x=year, y=avg))+
        geom_point()+
        geom_line()+
        labs(x="Year", y="Average Temperature")
    }
  })
  output$table1 <- renderTable({
    uah[sample(nrow(uah), 5),] 
  })
  output$table2 <- renderTable({
      uah %>%
      filter(year >= input$year[1],
             year <= input$year[2]) %>% 
      select(input$columns)
  })
}

shinyApp(ui = ui, server = server)