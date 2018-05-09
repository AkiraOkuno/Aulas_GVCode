##################################################################################################
#__________________________________SHINY_________________________________________________________

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("First Shiny App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("name", label = "Choose stocks to compare returns:", 
                     choices = c("Cisco", "Amazon", "Nasdaq", "MicroStrategy", "S&P500", "Apple", "IBM"),
                     multiple = T,
                     selected = "Cisco",
                     options = list(maxItems = 2, placeholder = 'select a name')
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      plotlyOutput("trendstocks"),
      br(),
      br(),
      plotlyOutput("pricestocks"),
      dataTableOutput("dataf")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$dataf <- renderDataTable(df)
  
  output$trendstocks <- renderPlotly({
    if( length(input$name) == 0){
      print("Please select at least one stock")
    }
    else{
      print(input$name)
      df_trend <- returnsdf[, c("date", input$name)]
      print(colnames(df_trend))
      
      if (ncol(df_trend) == 2){
        p <-  plot_ly(df_trend,x =~ date, y = df_trend[,2], name = input$name, mode = "lines", type = "scatter") %>%
          layout(title= "Stock Returns")
        p
      } 
      else{
        p <- plot_ly(df_trend,x =~ date, y = df_trend[,2], name = colnames(df_trend)[2], mode = "lines", type = "scatter") %>%  
          add_trace(y = df_trend[,3], name = colnames(df_trend)[3], mode = "lines") %>%
          layout(title = "Stock Returns")
        p
      }}})
  
  output$pricestocks <- renderPlotly({
    if( length(input$name) == 0){
      print("Please select at least one stock")
    }
    else{
      print(input$name)
      df_trend <- df[, c("date", input$name)]
      print(colnames(df_trend))
      
      if (ncol(df_trend) == 2){
        p <-  plot_ly(df_trend,x =~ date, y = df_trend[,2], color = "purple", name = input$name, mode = "lines", type = "scatter") %>%
          layout(title= "Stock Prices")
        p
      } 
      else{
        p <- plot_ly(df_trend,x =~ date, color = "purple", y = df_trend[,2], name = colnames(df_trend)[2], mode = "lines", type = "scatter") %>%  
          add_trace(y = df_trend[,3], name = colnames(df_trend)[3], color = "green", mode = "lines") %>%
          layout(title = "Stock Prices")
        p
      }}})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#LINKS UTEIS
#https://shiny.rstudio.com/
#https://plot.ly/r/shiny-gallery/
#https://moderndata.plot.ly/dashboards-in-r-with-shiny-plotly/
#https://rstudio.github.io/shinydashboard/
