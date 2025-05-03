library(shiny)
library(tidyverse)

library(tidyverse)
library(quantmod)

# Download TMUS data and prepare TMUS_DF
getSymbols("TMUS", from = "2015-01-01", to = Sys.Date(), auto.assign = TRUE)
divTMUS <- getDividends("TMUS", auto.assign = FALSE)

TMUS_xts <- TMUS
TMUS_merged <- merge(TMUS_xts, divTMUS)
TMUS_mergedDF <- data.frame(Date = index(TMUS_merged), coredata(TMUS_merged))

TMUS_DF <- TMUS_mergedDF %>%
  mutate(
    AvPrice = cummean(TMUS.Adjusted),
    volatility = map_dbl(seq_along(TMUS.Adjusted), ~ sd(TMUS.Adjusted[1:.x], na.rm = TRUE))
  )


ui <- fluidPage(
  titlePanel("TMUS Stock Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Click below to choose which plot to be visualized",
                  choices = c("Adjusted Price" = "TMUS.Adjusted",
                              "Volatility" = "volatility",
                              "Average Price" = "AvPrice"))
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

server <- function(input, output) {
  output$linePlot <- renderPlot({
    req(input$metric)
    req(TMUS_DF[[input$metric]])
    
    ggplot(TMUS_mergedDF, aes(x = Date, y = .data[[input$metric]])) +
      geom_line(color = "blue") +
      labs(title = paste("TMUS", input$metric), x = "Date", y = input$metric)
  })
}

shinyApp(ui = ui, server = server)
