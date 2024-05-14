library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(fable)
library(forecast)


data<-read.csv("fcst_plus_actuals_lng.csv")
models<-read.csv("fcst_lng.csv")
hts_concordance<-read.csv("hts_series_names_concordance.csv")
ind_concordance<-read.csv("ind_concordance.csv")


last_yr<-2022


ui <- fluidPage(
   
   titlePanel("Forecasting MTAGDP"),
   
   br(),
   
   h5("Author: RSBED"),
   
   br(),
   
   sidebarLayout(
      
      sidebarPanel(
         selectInput(
            inputId = "TA",
            label = "Select TA",
            choices = unique(data$TA),
            selected = "Wellington" ),
      selectInput(
            inputId = "Ind",
            label = "Select Industry",
            choices = unique(data$Ind),
            selected = "A" ),
      selectInput(
         inputId = "Region",
         label = "Select Region",
         choices = unique(data$Region))
      ),
      
      
      
      # Show the caption and forecast plots
      mainPanel(
         h3(textOutput("caption")),
         
         tabsetPanel(
            tabPanel("Forecast",fluidRow(
               column(6,plotOutput(outputId="plotAll")),  
               column(6,plotOutput(outputId="plotInd"))),
               htmlOutput("Text"),
               htmlOutput("Industry")), 
            tabPanel("Model Parameters",fluidRow(
               column(6,dataTableOutput(outputId="selection")))),
            tabPanel("Residual Diagnostics",fluidRow(
               plotOutput(outputId="residuals"))))
)))

server <- function(input, output, session) {
   
   observe({
      updateSelectInput(session, "Region", choices = as.character(data[data$TA==input$TA,"Region"]))
   })
   
   
   output$plotAll <- renderPlot({
         ggplot() +
         geom_line(data %>% filter(TA == input$TA & Region == input$Region & Year<=last_yr),mapping=aes(Year, value)) +
         geom_line(data %>% filter(TA == input$TA & Region == input$Region & Year>=last_yr),mapping=aes(Year, value),linetype="dashed") +
         geom_point(data %>% filter(TA == input$TA & Region == input$Region & Year>=last_yr),mapping=aes(Year, value)) +
         xlab("") +
         ylab("GDP") +
         facet_wrap(~Ind)
   })
   
   output$plotInd <- renderPlot({ 
         ggplot(data %>% filter(TA == input$TA & Ind == input$Ind & Region == input$Region & Year<=last_yr),mapping=aes(Year, value)) +
         geom_line(data %>% filter(TA == input$TA & Ind == input$Ind & Region == input$Region & Year>=last_yr),mapping=aes(Year, value),linetype="dashed") +
         geom_point(data %>% filter(TA == input$TA & Ind == input$Ind & Region == input$Region & Year>=last_yr),mapping=aes(Year, value)) +
         ylab("GDP") +
         facet_wrap(~Ind) +
         geom_line()
   })
   
   output$selection <- DT::renderDataTable({
      
      selection <- models %>% rename(RegionTA=TA)  %>%
                   left_join(hts_concordance[,c("hts_name","Region","TA")],by=c("RegionTA"="hts_name")) %>%
                   filter(TA == input$TA & Ind == input$Ind & Region == input$Region )%>% 
         select(-FcstMethod,-Date,-value,-Group,-RegionTA) %>%
         rename(p=non_seasonal_ar_order,
                d=non_seasonal_diff_order,
                q=non_seasonal_ma_order,
                P=seasonal_ar_order,
                D=seasonal_diff_order,
                Q=seasonal_ma_order,
                m=period_of_data) %>%
         select(Region,TA,Ind,p,d,q,P,D,Q,m)
      
      selection
      
   })
   
   
   output$Text<-renderPrint({
      
        selection <- models %>% rename(RegionTA=TA)  %>%
         left_join(hts_concordance[,c("hts_name","Region","TA")],by=c("RegionTA"="hts_name")) %>%
         filter(TA == input$TA & Ind == input$Ind  & Region == input$Region) %>% 
         select(-FcstMethod,-Date,-value,-Group,-RegionTA) %>%
         rename(p=non_seasonal_ar_order,
                d=non_seasonal_diff_order,
                q=non_seasonal_ma_order,
                P=seasonal_ar_order,
                D=seasonal_diff_order,
                Q=seasonal_ma_order,
                m=period_of_data) %>%
          select(Region,TA,Ind,p,d,q,P,D,Q,m)

         
         HTML("<b>",paste0("ARIMA(",selection$p,",",selection$d,",",selection$q,")","(",selection$P,",",selection$D,",",selection$Q,")"),"<sub>",selection$m,"</sub>","</b>")
      })
   
   output$Industry<-renderPrint({
      
      industry_name<-ind_concordance %>% filter(Ind==input$Ind) %>% select(Ind_name) %>% as.character
      
      
      HTML("<h2>",industry_name,"</h2>")
   })
   
   
   output$residuals <- renderPlot({ 
      
      selection <- models %>% rename(RegionTA=TA)  %>%
         left_join(hts_concordance[,c("hts_name","Region","TA")],by=c("RegionTA"="hts_name")) %>%
         filter(TA == input$TA & Ind == input$Ind  & Region == input$Region) %>% 
         select(-FcstMethod,-Date,-value,-Group,-RegionTA) %>%
         rename(p=non_seasonal_ar_order,
                d=non_seasonal_diff_order,
                q=non_seasonal_ma_order,
                P=seasonal_ar_order,
                D=seasonal_diff_order,
                Q=seasonal_ma_order,
                m=period_of_data) %>%
         select(Region,TA,Ind,p,d,q,P,D,Q,m)
      
      data_ts<-ts(data %>% filter(TA == input$TA & Ind == input$Ind  & Region == input$Region & Year<last_yr) %>% select(value),start=c(2009))
      arima_ts<-arima(data_ts,order=c(selection$p,selection$d,selection$q))
      checkresiduals(arima_ts)
      
      # arima_ts2<-data_ts %>% as_tsibble() %>% model(arima=ARIMA(value ~ pdq(selection$p,selection$d,selection$q) + PDQ(selection$P,selection$D,selection$Q)))
      # arima_ts2 %>%  feasts::gg_tsresiduals()  + ylab("residuals")
      
   })

   
   
}

shinyApp(ui, server)