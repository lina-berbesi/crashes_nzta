
.rs.restartR()

lapply(c("shiny","ggplot2","dplyr"),require,character.only=TRUE)


data<-read.csv("crash_timeseries_nzta.csv") %>% filter(crashSever != "Fatal Crash")

ui <- fluidPage(
   
   titlePanel("Time Series on Traffic Accidents - NZTA"),
   
   br(),
   
   h5("Author: Lina Berbesi"),
   
   br(),
   
   sidebarLayout(
      
      sidebarPanel(
         selectInput(
            inputId = "TA",
            label = "Select TA",
            choices = unique(data$taname),
            selected = "Rotorua District" ),
      selectInput(
            inputId = "Severity",
            label = "Select Severity",
            choices = unique(data$crashSever),
            selected = "Minor Crash" ),
      selectInput(
         inputId = "Region",
         label = "Select Region",
         choices = unique(data$regname),
         selected = "Bay of Plenty Region" )
      ),
      
      
      
      # Show the caption and forecast plots
      mainPanel(
         h3(textOutput("caption")),
         
         tabsetPanel(
            tabPanel("Forecast",fluidRow(
               column(6,plotOutput(outputId="plotAll")),  
               column(6,plotOutput(outputId="plotTA")))))
)))

server <- function(input, output, session) {
   
   observe({
      updateSelectInput(session, "Region", choices = as.character(data[data$taname==input$TA,"regname"]))
   })
   
   
   output$plotAll <- renderPlot({
         ggplot() +
         geom_line(data %>% filter(regname == input$Region & crashSever == input$Severity ),mapping=aes(x=crashYear, y=crashcnt)) +
         geom_point(data %>% filter(regname == input$Region & crashSever == input$Severity ),mapping=aes(x=crashYear, y=crashcnt)) +
         xlab("") +
         ylab("Traffic Accidents Count") +
         facet_wrap(~taname)
   })
   
   output$plotTA <- renderPlot({ 
         ggplot() +
         geom_line(data %>% filter(taname == input$TA & crashSever == input$Severity & regname == input$Region),mapping=aes(x=crashYear, y=crashcnt)) +
         geom_point(data %>% filter(taname == input$TA &  crashSever == input$Severity & regname == input$Region),mapping=aes(x=crashYear, y=crashcnt)) +
         ylab("Traffic Accidents Count") 
   })
   
   
}

shinyApp(ui, server)