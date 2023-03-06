library(shiny)
library(tidyverse)
getwd()
UAH <- read_delim("UAH-lower-troposphere-long.csv.bz2")

length(unique(UAH$region))

 ui <- fluidPage(
  #TITLE:
  titlePanel("Exploring Temperature Over Regions and Time"),
  mainPanel(
    tabsetPanel(
      #ABOUT SECTION
      tabPanel("About",
          sidebarLayout(
            #sidebar with info about data set:
            sidebarPanel(
              p("This dataset contains", nrow(UAH), "observations and", ncol(UAH), 
                "variables"),
              p("Of all the data, the ", strong("year"), "ranges from", min(UAH$year), 
                "to", max(UAH$year),),
              p("The temperature variable,", em("temp"), "is measured as the deviation 
                from the baseline over 1991 to 2020 (in degrees Celsius)"),
              p("The table to the right is a", strong("random sample"), "of 8 rows 
                from the dataset")
            ),
            #main panel with 8 sample rows of the data:
            mainPanel(tableOutput("sample"))
          )
               ),
      
      #PLOT SECTION
      tabPanel("Plot",
          sidebarLayout(
            #choose color of scatter plot:
            sidebarPanel(fluidRow(column(6,
                                          radioButtons("color", "Choose color",
                                                       choices = c("blue", "red", "green")))
                                          ),
                         checkboxGroupInput("plotregions", "Please choose region(s) 
                                            in order to see the plot",
                                            choices = unique(UAH$region))
                         ),
            
            #We want plot
            mainPanel(
              textOutput("plot_text"),
              plotOutput("plot"))
          )     
               ),
      
      #TABLE SECTION
      tabPanel("Table",
          sidebarLayout(
            sidebarPanel(
              p("Select both a region and a month to see table."),
              selectInput("regions", "Please choose region",
                          choices = UAH$region),
              selectInput("months", "Please choose month(s)",
                          choices = UAH$month,
                          multiple = TRUE)
            ),
            mainPanel(
              textOutput("max"),
              tableOutput("table")
            )
          )     
               )
    )
  )
)

server <- function(input, output){
  #ABOUT SECTION
  output$sample <- renderTable({
    UAH %>% sample_n(8)
  })
  
  #PLOT SECTION
  #scatter plot, year vs temp, 
  #option to see different regions by color (with check box) 
  #option to see different months by radio buttons
  regionplot_filter <- reactive({
    UAH %>% 
      filter(region %in% input$plotregions)
  })
  
  output$plot <- renderPlot({
    regionplot_filter() %>% 
      ggplot(aes(year, temp))+
      geom_point(col = input$color)+
      labs(title= "Temperature Deviation Over the Years",
           x= "Year",
           y= "Temperature Deviation")
  })
  
  output$plot_text <- renderText({
    regionplot_filter() %>% 
      pull(temp) %>% 
      mean() %>% 
      paste("The average temperature for the region(s) over the years:", .)
  })

  #TABLE SECTION
  #select which region to see data for that region
  regionfilter <- reactive ({
    UAH %>% 
      filter(region %in% input$regions) %>% 
      filter(month %in% input$months)
  })
 
  output$table <- renderTable({
    regionfilter()
  }) 
  
  output$max <- renderText({
    regionfilter() %>% 
      pull(temp) %>% 
      max() %>% 
      paste("The hottest temperature deviation from this region:", .)
  })
  
}


#Run The App
shinyApp(ui = ui, server = server)